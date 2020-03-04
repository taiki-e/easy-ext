//! An attribute macro for easily writing [extension trait pattern](https://github.com/rust-lang/rfcs/blob/master/text/0445-extension-trait-conventions.md).
//!
//! ## Examples
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext(ResultExt)]
//! impl<T, E> Result<T, E> {
//!     fn err_into<U>(self) -> Result<T, U>
//!     where
//!         E: Into<U>,
//!     {
//!         self.map_err(Into::into)
//!     }
//! }
//! ```
//!
//! Code like this will be generated:
//!
//! ```rust
//! trait ResultExt<T, E> {
//!     fn err_into<U>(self) -> Result<T, U>
//!     where
//!         E: Into<U>;
//! }
//!
//! impl<T, E> ResultExt<T, E> for Result<T, E> {
//!     fn err_into<U>(self) -> Result<T, U>
//!     where
//!         E: Into<U>,
//!     {
//!         self.map_err(Into::into)
//!     }
//! }
//! ```
//!
//! ### Supported items
//!
//! * [Methods](https://doc.rust-lang.org/book/ch05-03-method-syntax.html)
//!
//! * [Associated constants](https://rust-lang-nursery.github.io/edition-guide/rust-2018/trait-system/associated-constants.html)

#![doc(html_root_url = "https://docs.rs/easy-ext/0.1.6")]
#![doc(test(
    no_crate_inject,
    attr(deny(warnings, rust_2018_idioms, single_use_lifetimes), allow(dead_code))
))]
#![forbid(unsafe_code)]
#![warn(rust_2018_idioms, unreachable_pub)]
// It cannot be included in the published code because these lints have false positives in the minimum required version.
#![cfg_attr(test, warn(single_use_lifetimes))]
#![warn(clippy::all)]
// mem::take requires Rust 1.40
#![allow(clippy::mem_replace_with_default)]

// older compilers require explicit `extern crate`.
#[allow(unused_extern_crates)]
extern crate proc_macro;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    mem,
};

use proc_macro::{Delimiter, Spacing, TokenStream, TokenTree};
use quote::{format_ident, ToTokens};
use syn::{punctuated::Punctuated, *};

macro_rules! error {
    ($span:expr, $msg:expr) => {
        Err(syn::Error::new_spanned(&$span, $msg))
    };
    ($span:expr, $($tt:tt)*) => {
        error!($span, format!($($tt)*))
    };
}

/// An attribute macro for easily writing [extension trait pattern](https://github.com/rust-lang/rfcs/blob/master/text/0445-extension-trait-conventions.md).
///
/// ## Examples
///
/// ```rust
/// use easy_ext::ext;
///
/// #[ext(ResultExt)]
/// impl<T, E> Result<T, E> {
///     fn err_into<U>(self) -> Result<T, U>
///     where
///         E: Into<U>,
///     {
///         self.map_err(Into::into)
///     }
/// }
/// ```
///
/// Code like this will be generated:
///
/// ```rust
/// trait ResultExt<T, E> {
///     fn err_into<U>(self) -> Result<T, U>
///     where
///         E: Into<U>;
/// }
///
/// impl<T, E> ResultExt<T, E> for Result<T, E> {
///     fn err_into<U>(self) -> Result<T, U>
///     where
///         E: Into<U>,
///     {
///         self.map_err(Into::into)
///     }
/// }
/// ```
///
/// ### Supported items
///
/// * [Methods](https://doc.rust-lang.org/book/ch05-03-method-syntax.html)
///
/// * [Associated constants](https://rust-lang-nursery.github.io/edition-guide/rust-2018/trait-system/associated-constants.html)
///
/// ### Visibility
///
/// * The generated extension trait inherits the visibility of the item in the original `impl`.
///
/// * The visibility of all the items in the original `impl` must be identical.
#[proc_macro_attribute]
pub fn ext(args: TokenStream, input: TokenStream) -> TokenStream {
    let ext_ident = match syn::parse_macro_input!(args) {
        None => format_ident!("__ExtTrait{}", hash(&input)),
        Some(ext_ident) => ext_ident,
    };

    let mut item: ItemImpl = syn::parse_macro_input!(input);

    trait_from_item(&mut item, ext_ident)
        .map(ToTokens::into_token_stream)
        .map(|mut tokens| {
            tokens.extend(item.into_token_stream());
            tokens
        })
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

fn trait_from_item(item: &mut ItemImpl, ident: Ident) -> Result<ItemTrait> {
    let generics = item.generics.clone();
    let ty_generics = generics.split_for_impl().1;
    let trait_ = parse_quote!(#ident #ty_generics);
    item.trait_ = Some((None, trait_, token::For::default()));

    let mut vis = None;
    let mut items = Vec::with_capacity(item.items.len());
    item.items.iter_mut().try_for_each(|item| {
        trait_item_from_impl_item(item, &mut vis).map(|item| items.push(item))
    })?;

    let mut attrs = item.attrs.clone();
    attrs.push(parse_quote!(#[allow(patterns_in_fns_without_body)])); // mut self

    Ok(ItemTrait {
        attrs,
        vis: vis.unwrap_or(Visibility::Inherited),
        unsafety: item.unsafety,
        auto_token: None,
        trait_token: token::Trait::default(),
        ident,
        generics,
        colon_token: None,
        supertraits: Punctuated::new(),
        brace_token: token::Brace::default(),
        items,
    })
}

fn trait_item_from_impl_item(
    impl_item: &mut ImplItem,
    prev: &mut Option<Visibility>,
) -> Result<TraitItem> {
    fn check_visibility(
        current: Visibility,
        prev: &mut Option<Visibility>,
        span: &dyn ToTokens,
    ) -> Result<()> {
        match prev {
            None => *prev = Some(current),
            Some(prev) if *prev == current => {}
            Some(prev) => {
                if let Visibility::Inherited = prev {
                    return error!(current, "All items must have inherited visibility");
                } else {
                    return error!(
                        if let Visibility::Inherited = current { span } else { &current },
                        "All items must have a visibility of `{}`",
                        prev.to_token_stream(),
                    );
                }
            }
        }
        Ok(())
    }

    match impl_item {
        ImplItem::Const(item) => {
            let vis = mem::replace(&mut item.vis, Visibility::Inherited);
            check_visibility(vis, prev, &item.ident)?;
            Ok(TraitItem::Const(from_const(item)))
        }
        ImplItem::Method(item) => {
            let vis = mem::replace(&mut item.vis, Visibility::Inherited);
            check_visibility(vis, prev, &item.sig.ident)?;
            Ok(TraitItem::Method(from_method(item)))
        }
        _ => error!(impl_item, "unsupported item"),
    }
}

fn from_const(impl_const: &ImplItemConst) -> TraitItemConst {
    TraitItemConst {
        attrs: impl_const.attrs.clone(),
        const_token: token::Const::default(),
        ident: impl_const.ident.clone(),
        colon_token: token::Colon::default(),
        ty: impl_const.ty.clone(),
        default: None,
        semi_token: token::Semi::default(),
    }
}

fn from_method(impl_method: &ImplItemMethod) -> TraitItemMethod {
    let mut attrs = impl_method.attrs.clone();
    find_remove(&mut attrs, "inline"); // clippy::inline_fn_without_body

    TraitItemMethod {
        attrs,
        sig: impl_method.sig.clone(),
        default: None,
        semi_token: Some(token::Semi::default()),
    }
}

fn find_remove(attrs: &mut Vec<Attribute>, ident: &str) -> Option<Attribute> {
    attrs.iter().position(|attr| attr.path.is_ident(ident)).map(|i| attrs.remove(i))
}

// =================================================================================================
// Hash

/// Returns the hash value of the input AST.
fn hash(tokens: &TokenStream) -> u64 {
    let tokens = TokenStreamHelper(tokens);
    let mut hasher = DefaultHasher::new();
    tokens.hash(&mut hasher);
    hasher.finish()
}

// Based on https://github.com/dtolnay/syn/blob/1.0.5/src/tt.rs

struct TokenTreeHelper<'a>(&'a TokenTree);

impl Hash for TokenTreeHelper<'_> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        match self.0 {
            TokenTree::Group(g) => {
                0_u8.hash(h);
                match g.delimiter() {
                    Delimiter::Parenthesis => 0_u8.hash(h),
                    Delimiter::Brace => 1_u8.hash(h),
                    Delimiter::Bracket => 2_u8.hash(h),
                    Delimiter::None => 3_u8.hash(h),
                }

                for tt in g.stream() {
                    TokenTreeHelper(&tt).hash(h);
                }
                0xff_u8.hash(h); // terminator w/ a variant we don't normally hash
            }
            TokenTree::Punct(op) => {
                1_u8.hash(h);
                op.as_char().hash(h);
                match op.spacing() {
                    Spacing::Alone => 0_u8.hash(h),
                    Spacing::Joint => 1_u8.hash(h),
                }
            }
            TokenTree::Literal(lit) => (2_u8, lit.to_string()).hash(h),
            TokenTree::Ident(word) => (3_u8, word.to_string()).hash(h),
        }
    }
}

struct TokenStreamHelper<'a>(&'a TokenStream);

impl Hash for TokenStreamHelper<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let tokens = self.0.clone().into_iter().collect::<Vec<_>>();
        tokens.len().hash(state);
        for tt in tokens {
            TokenTreeHelper(&tt).hash(state);
        }
    }
}
