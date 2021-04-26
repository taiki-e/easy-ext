//! An attribute macro for easily writing [extension trait pattern][rfc0445].
//!
//! # Examples
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext(ResultExt)]
//! impl<T, E> Result<T, E> {
//!     pub fn err_into<U>(self) -> Result<T, U>
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
//! pub trait ResultExt<T, E> {
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
//! You can elide the trait name.
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext]
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
//! Note that in this case, `#[ext]` assigns a random name, so you cannot
//! import/export the generated trait.
//!
//! ## Visibility
//!
//! There are two ways to specify visibility.
//!
//! ### Impl-level visibility
//!
//! The first way is to specify visibility as the first argument to the `#[ext]`
//! attribute. For example:
//!
//! ```rust
//! use easy_ext::ext;
//!
//! // unnamed
//! #[ext(pub)]
//! impl str {
//!     fn foo(&self) {}
//! }
//!
//! // named
//! #[ext(pub StrExt)]
//! impl str {
//!     fn bar(&self) {}
//! }
//! ```
//!
//! ### Associated-item-level visibility
//!
//! Another way is to specify visibility at the associated item level.
//!
//! For example, if the method is `pub` then the trait will also be `pub`:
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext(ResultExt)] // generate `pub trait ResultExt`
//! impl<T, E> Result<T, E> {
//!     pub fn err_into<U>(self) -> Result<T, U>
//!     where
//!         E: Into<U>,
//!     {
//!         self.map_err(Into::into)
//!     }
//! }
//! ```
//!
//! This is useful when migrate from an inherent impl to an extension trait.
//!
//! Note that the visibility of all the associated items in the `impl` must be identical.
//!
//! Note that you cannot specify impl-level visibility and associated-item-level visibility at the same time.
//!
//! ## [Supertraits](https://doc.rust-lang.org/reference/items/traits.html#supertraits)
//!
//! If you want the extension trait to be a subtrait of another trait,
//! add `Self: SubTrait` bound to the `where` clause.
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext(Ext)]
//! impl<T> T
//! where
//!     Self: Default,
//! {
//!     fn method(&self) {}
//! }
//! ```
//!
//! ## Supported items
//!
//! ### [Associated functions (methods)](https://doc.rust-lang.org/reference/items/associated-items.html#associated-functions-and-methods)
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext]
//! impl<T> T {
//!     fn method(&self) {}
//! }
//! ```
//!
//! ### [Associated constants](https://doc.rust-lang.org/reference/items/associated-items.html#associated-constants)
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext]
//! impl<T> T {
//!     const MSG: &'static str = "Hello!";
//! }
//! ```
//!
//! ### [Associated types](https://doc.rust-lang.org/reference/items/associated-items.html#associated-types)
//!
//! ```rust
//! use easy_ext::ext;
//!
//! #[ext]
//! impl str {
//!     type Owned = String;
//!
//!     fn method(&self) -> Self::Owned {
//!         self.to_owned()
//!     }
//! }
//! ```
//!
//! [rfc0445]: https://github.com/rust-lang/rfcs/blob/HEAD/text/0445-extension-trait-conventions.md

#![doc(test(
    no_crate_inject,
    attr(
        deny(warnings, rust_2018_idioms, single_use_lifetimes),
        allow(dead_code, unused_variables)
    )
))]
#![forbid(unsafe_code)]
#![warn(future_incompatible, rust_2018_idioms, unreachable_pub)]
// It cannot be included in the published code because these lints have false positives in the minimum required version.
#![cfg_attr(test, warn(single_use_lifetimes))]
#![warn(clippy::default_trait_access)]

// older compilers require explicit `extern crate`.
#[allow(unused_extern_crates)]
extern crate proc_macro;

macro_rules! error {
    ($span:expr, $msg:expr) => {
        syn::Error::new_spanned(&$span, $msg)
    };
    ($span:expr, $($tt:tt)*) => {
        error!($span, format!($($tt)*))
    };
}

mod ast;
mod iter;

use std::{collections::hash_map::DefaultHasher, hash::Hasher, iter::FromIterator, mem};

use proc_macro::TokenStream;
use proc_macro2::{Group, Spacing, Span, TokenStream as TokenStream2, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    token, Error, Ident, Result,
};

use crate::ast::{
    Attribute, AttributeKind, GenericParam, Generics, ImplItem, ItemImpl, ItemTrait, Signature,
    TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, TypeParam, Visibility,
};

/// An attribute macro for easily writing [extension trait pattern][rfc0445].
///
/// See crate level documentation for details.
///
/// [rfc0445]: https://github.com/rust-lang/rfcs/blob/HEAD/text/0445-extension-trait-conventions.md
#[proc_macro_attribute]
pub fn ext(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut args: Args = syn::parse_macro_input!(args);
    if args.name.is_none() {
        args.name = Some(Ident::new(&format!("__ExtTrait{}", hash(&input)), Span::call_site()));
    }

    let mut item: ItemImpl = syn::parse_macro_input!(input);

    trait_from_impl(&mut item, args)
        .map(ToTokens::into_token_stream)
        .map(|mut tokens| {
            tokens.extend(item.into_token_stream());
            tokens
        })
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

struct Args {
    // impl-level visibility
    vis: Option<Visibility>,
    // trait name
    name: Option<Ident>,
}

impl Parse for Args {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let name: Option<Ident> = input.parse()?;
        Ok(Args { vis: if vis.is_inherited() { None } else { Some(vis) }, name })
    }
}

fn determine_trait_generics<'a>(
    generics: &mut Generics,
    self_ty: &'a [TokenTree],
) -> Option<&'a Ident> {
    if self_ty.len() != 1 {
        return None;
    }
    if let TokenTree::Ident(ident) = &self_ty[0] {
        let i = generics.params.iter().position(|(param, _)| {
            if let GenericParam::Type(param) = param { param.ident == *ident } else { false }
        });
        if let Some(i) = i {
            let mut params =
                mem::replace(&mut generics.params, Vec::new()).into_iter().collect::<Vec<_>>();
            let (param, _) = params.remove(i);
            generics.params = params.into_iter().collect();

            if let GenericParam::Type(TypeParam {
                colon_token: Some(colon_token), bounds, ..
            }) = param
            {
                let bounds =
                    bounds.iter().filter(|(b, _)| !b.is_maybe).map(|(b, p)| quote! { #b #p });
                generics.make_where_clause().extend(quote! {
                    Self #colon_token #(#bounds)*
                });
            }

            return Some(ident);
        }
    }
    None
}

fn trait_from_impl(item: &mut ItemImpl, args: Args) -> Result<ItemTrait> {
    /// Replace `self_ty` with `Self`.
    struct ReplaceParam<'a> {
        self_ty: &'a Ident,
    }

    impl ReplaceParam<'_> {
        fn visit_token_stream(&self, tokens: &mut TokenStream2) -> bool {
            let mut out: Vec<TokenTree> = Vec::new();
            let mut modified = false;
            let mut iter = tokens.clone().into_iter().peekable();
            while let Some(tt) = iter.next() {
                match tt {
                    TokenTree::Ident(ident) => {
                        if ident == *self.self_ty {
                            modified = true;
                            let self_ = Ident::new("Self", ident.span());
                            match iter.peek() {
                                Some(TokenTree::Punct(p))
                                    if p.as_char() == ':' && p.spacing() == Spacing::Joint =>
                                {
                                    let next = iter.next().unwrap();
                                    match iter.peek() {
                                        Some(TokenTree::Punct(p)) if p.as_char() == ':' => {
                                            let span = ident.span();
                                            out.extend(quote_spanned!(span=> <#self_>))
                                        }
                                        _ => out.push(self_.into()),
                                    }
                                    out.push(next);
                                }
                                _ => out.push(self_.into()),
                            }
                        } else {
                            out.push(TokenTree::Ident(ident));
                        }
                    }
                    TokenTree::Group(group) => {
                        let mut content = group.stream();
                        modified |= self.visit_token_stream(&mut content);
                        let mut new = Group::new(group.delimiter(), content);
                        new.set_span(group.span());
                        out.push(TokenTree::Group(new));
                    }
                    other => out.push(other),
                }
            }
            if modified {
                *tokens = TokenStream2::from_iter(out);
            }
            modified
        }

        // Everything below is simply traversing the syntax tree.

        fn visit_trait_item_mut(&mut self, node: &mut TraitItem) {
            match node {
                TraitItem::Const(node) => {
                    self.visit_token_stream(&mut node.ty);
                }
                TraitItem::Method(node) => {
                    self.visit_signature_mut(&mut node.sig);
                }
                TraitItem::Type(node) => {
                    self.visit_generics_mut(&mut node.generics);
                }
            }
        }

        fn visit_signature_mut(&mut self, node: &mut Signature) {
            self.visit_generics_mut(&mut node.generics);
            self.visit_token_stream(&mut node.inputs);
            if let Some(ty) = &mut node.output {
                self.visit_token_stream(ty);
            }
        }

        fn visit_generics_mut(&mut self, generics: &mut Generics) {
            for (param, _) in &mut generics.params {
                match param {
                    GenericParam::Type(param) => {
                        for (bound, _) in &mut param.bounds {
                            self.visit_token_stream(&mut bound.tokens);
                        }
                    }
                    GenericParam::Const(_) | GenericParam::Lifetime(_) => {}
                }
            }
            self.visit_token_stream(&mut generics.where_clause);
        }
    }

    let name = args.name.unwrap();
    let mut generics = item.generics.clone();
    let mut visitor = determine_trait_generics(&mut generics, &item.self_ty)
        .map(|self_ty| ReplaceParam { self_ty });

    if let Some(visitor) = &mut visitor {
        visitor.visit_generics_mut(&mut generics);
    }
    let ty_generics = generics.ty_generics();
    item.trait_ = Some(quote! { #name #ty_generics for });

    // impl-level visibility
    let impl_vis = args.vis;
    // assoc-item-level visibility
    let mut assoc_vis = None;
    let mut items = Vec::with_capacity(item.items.len());
    item.items.iter_mut().try_for_each(|item| {
        trait_item_from_impl_item(item, &mut assoc_vis, &impl_vis).map(|mut item| {
            if let Some(visitor) = &mut visitor {
                visitor.visit_trait_item_mut(&mut item);
            }
            items.push(item)
        })
    })?;

    let mut attrs = item.attrs.clone();
    find_remove(&mut item.attrs, AttributeKind::Doc); // https://github.com/taiki-e/easy-ext/issues/20
    attrs.push(Attribute::new(quote!(allow(patterns_in_fns_without_body)))); // mut self

    Ok(ItemTrait {
        attrs,
        // priority: impl-level visibility > assoc-item-level visibility > inherited visibility
        vis: impl_vis.unwrap_or_else(|| assoc_vis.unwrap_or(Visibility::Inherited)),
        unsafety: item.unsafety.clone(),
        trait_token: Ident::new("trait", item.impl_token.span()),
        ident: name,
        generics,
        brace_token: token::Brace(item.brace_token.span),
        items,
    })
}

fn trait_item_from_impl_item(
    impl_item: &mut ImplItem,
    prev_vis: &mut Option<Visibility>,
    impl_vis: &Option<Visibility>,
) -> Result<TraitItem> {
    fn check_visibility(
        current: Visibility,
        prev: &mut Option<Visibility>,
        impl_vis: &Option<Visibility>,
        span: &dyn ToTokens,
    ) -> Result<()> {
        if impl_vis.is_some() {
            return if current.is_inherited() {
                Ok(())
            } else {
                Err(error!(current, "all associated items must have inherited visibility"))
            };
        }
        match prev {
            None => *prev = Some(current),
            Some(prev) if *prev == current => {}
            Some(prev) => {
                return if prev.is_inherited() {
                    Err(error!(current, "all associated items must have inherited visibility"))
                } else {
                    Err(error!(
                        if current.is_inherited() { span } else { &current },
                        "all associated items must have a visibility of `{}`", prev,
                    ))
                };
            }
        }
        Ok(())
    }

    match impl_item {
        ImplItem::Const(impl_const) => {
            let vis = mem::replace(&mut impl_const.vis, Visibility::Inherited);
            check_visibility(vis, prev_vis, impl_vis, &impl_const.ident)?;

            let attrs = impl_const.attrs.clone();
            find_remove(&mut impl_const.attrs, AttributeKind::Doc); // https://github.com/taiki-e/easy-ext/issues/20
            Ok(TraitItem::Const(TraitItemConst {
                attrs,
                const_token: impl_const.const_token.clone(),
                ident: impl_const.ident.clone(),
                colon_token: impl_const.colon_token.clone(),
                ty: impl_const.ty.clone(),
                semi_token: impl_const.semi_token.clone(),
            }))
        }
        ImplItem::Type(impl_type) => {
            let vis = mem::replace(&mut impl_type.vis, Visibility::Inherited);
            check_visibility(vis, prev_vis, impl_vis, &impl_type.ident)?;

            let attrs = impl_type.attrs.clone();
            find_remove(&mut impl_type.attrs, AttributeKind::Doc); // https://github.com/taiki-e/easy-ext/issues/20
            Ok(TraitItem::Type(TraitItemType {
                attrs,
                type_token: impl_type.type_token.clone(),
                ident: impl_type.ident.clone(),
                generics: impl_type.generics.clone(),
                semi_token: impl_type.semi_token.clone(),
            }))
        }
        ImplItem::Method(impl_method) => {
            let vis = mem::replace(&mut impl_method.vis, Visibility::Inherited);
            check_visibility(vis, prev_vis, impl_vis, &impl_method.sig.ident)?;

            let mut attrs = impl_method.attrs.clone();
            find_remove(&mut impl_method.attrs, AttributeKind::Doc); // https://github.com/taiki-e/easy-ext/issues/20
            find_remove(&mut attrs, AttributeKind::Inline); // `#[inline]` is ignored on function prototypes
            Ok(TraitItem::Method(TraitItemMethod {
                attrs,
                sig: impl_method.sig.clone(),
                semi_token: ast::printing::punct(';', impl_method.body.span()),
            }))
        }
    }
}

fn find_remove(attrs: &mut Vec<Attribute>, kind: AttributeKind) {
    while let Some(i) = attrs.iter().position(|attr| attr.kind == kind) {
        attrs.remove(i);
    }
}

/// Returns the hash value of the input AST.
fn hash(input: &TokenStream) -> u64 {
    let mut hasher = DefaultHasher::new();
    hasher.write(input.to_string().as_bytes());
    hasher.finish()
}
