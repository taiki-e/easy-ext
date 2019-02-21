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

#![doc(html_root_url = "https://docs.rs/easy-ext/0.1.0")]
#![deny(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unreachable_pub)]

extern crate proc_macro;

use std::mem;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, Ident, ImplItem, ImplItemConst, ImplItemMethod, ImplItemType,
    ItemImpl, ItemTrait, TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, Visibility,
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
    let mut input_impl: ItemImpl = parse_macro_input!(input);
    let ext_ident: Ident = parse_macro_input!(args);

    let mut item = trait_from_item(&mut input_impl, ext_ident).into_token_stream();
    item.extend(input_impl.into_token_stream());
    TokenStream::from(item)
}

fn trait_from_item(item_impl: &mut ItemImpl, ident: Ident) -> ItemTrait {
    let generics = item_impl.generics.clone();
    let trait_ = parse_quote!(#ident #generics);
    item_impl.trait_ = Some((None, trait_, default()));

    let mut vis = None;
    let mut items = Vec::with_capacity(item_impl.items.len());
    item_impl.items.iter_mut().for_each(|item| {
        items.push(trait_item_from_impl_item(item, |_vis| match &vis {
            Some(v) if *v == _vis => {}
            Some(_) => panic!("visibility mismatch"),
            None => vis = Some(_vis),
        }))
    });

    ItemTrait {
        attrs: item_impl.attrs.clone(),
        vis: vis.unwrap_or(Visibility::Inherited),
        unsafety: item_impl.unsafety,
        auto_token: None,
        trait_token: default(),
        ident,
        generics,
        colon_token: None,
        supertraits: default(),
        brace_token: default(),
        items,
    }
}

fn trait_item_from_impl_item(impl_item: &mut ImplItem, f: impl FnOnce(Visibility)) -> TraitItem {
    macro_rules! from {
        ($($v:ident => $i:ident,)*) => {
            match impl_item {
                $(ImplItem::$v(item) => {
                    let vis = mem::replace(&mut item.vis, Visibility::Inherited);
                    item.defaultness = None;
                    f(vis);
                    TraitItem::$v($i(item))
                })*
                _ => panic!("unsupported item"),
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
        type_token: default(),
        ident: impl_type.ident.clone(),
        generics: impl_type.generics.clone(),
        colon_token: None,
        bounds: default(),
        default: None,
        semi_token: default(),
    }
}

fn const_from_const(impl_const: &ImplItemConst) -> TraitItemConst {
    TraitItemConst {
        attrs: impl_const.attrs.clone(),
        const_token: default(),
        ident: impl_const.ident.clone(),
        colon_token: default(),
        ty: impl_const.ty.clone(),
        default: None,
        semi_token: default(),
    }
}

fn method_from_method(impl_method: &ImplItemMethod) -> TraitItemMethod {
    TraitItemMethod {
        attrs: impl_method.attrs.clone(),
        sig: impl_method.sig.clone(),
        default: None,
        semi_token: Some(default()),
    }
}

fn default<T: Default>() -> T {
    T::default()
}
