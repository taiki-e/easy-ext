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
//!

#![doc(html_root_url = "https://docs.rs/easy-ext/0.1.4")]
#![doc(test(attr(deny(warnings), allow(dead_code, unused_assignments, unused_variables))))]
#![warn(unsafe_code)]
#![warn(rust_2018_idioms, unreachable_pub)]
#![warn(single_use_lifetimes)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::result_map_unwrap_or_else)]

extern crate proc_macro;

use std::mem;

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{
    parse_quote, punctuated::Punctuated, token, Attribute, Ident, ImplItem, ImplItemConst,
    ImplItemMethod, ImplItemType, ItemImpl, ItemTrait, Result, TraitItem, TraitItemConst,
    TraitItemMethod, TraitItemType, Visibility,
};

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
///
#[proc_macro_attribute]
pub fn ext(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item: ItemImpl = syn::parse_macro_input!(input);
    let ext_ident: Ident = syn::parse_macro_input!(args);

    let mut tts = trait_from_item(&mut item, ext_ident)
        .map(ToTokens::into_token_stream)
        .unwrap_or_else(|e| e.to_compile_error());

    tts.extend(item.into_token_stream());
    TokenStream::from(tts)
}

fn trait_from_item(item: &mut ItemImpl, ident: Ident) -> Result<ItemTrait> {
    let generics = item.generics.clone();
    let ty_generics = generics.split_for_impl().1;
    let trait_ = parse_quote!(#ident #ty_generics);
    item.trait_ = Some((None, trait_, token::For::default()));

    let mut vis = None;
    let mut items = Vec::with_capacity(item.items.len());
    item.items.iter_mut().try_for_each(|item| {
        trait_item_from_impl_item(item, |v| match &vis {
            Some(x) if *x == v => {}
            Some(_) => panic!("visibility mismatch"),
            None => vis = Some(v),
        })
        .map(|item| items.push(item))
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
    f: impl FnOnce(Visibility),
) -> Result<TraitItem> {
    macro_rules! from {
        ($($v:ident => $i:ident,)*) => {
            match impl_item {
                $(ImplItem::$v(item) => {
                    let vis = mem::replace(&mut item.vis, Visibility::Inherited);
                    item.defaultness = None;
                    f(vis);
                    Ok(TraitItem::$v($i(item)))
                })*
                _ => Err(syn::Error::new_spanned(impl_item, "unsupported item")),
            }
        };
    }

    from! {
        Const => const_from_const,
        Method => method_from_method,
        Type => type_from_type,
    }
}

fn type_from_type(impl_type: &ImplItemType) -> TraitItemType {
    TraitItemType {
        attrs: impl_type.attrs.clone(),
        type_token: token::Type::default(),
        ident: impl_type.ident.clone(),
        generics: impl_type.generics.clone(),
        colon_token: None,
        bounds: Punctuated::new(),
        default: None,
        semi_token: token::Semi::default(),
    }
}

fn const_from_const(impl_const: &ImplItemConst) -> TraitItemConst {
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

fn method_from_method(impl_method: &ImplItemMethod) -> TraitItemMethod {
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
