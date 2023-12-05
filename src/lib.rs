// SPDX-License-Identifier: Apache-2.0 OR MIT

/*!
<!-- tidy:crate-doc:start -->
A lightweight attribute macro for easily writing [extension trait pattern][rfc0445].

```toml
[dependencies]
easy-ext = "1"
```

*Compiler support: requires rustc 1.31+*

## Examples

```rust
use easy_ext::ext;

#[ext(ResultExt)]
pub impl<T, E> Result<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

Code like this will be generated:

```rust
pub trait ResultExt<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

You can elide the trait name.

```rust
use easy_ext::ext;

#[ext]
impl<T, E> Result<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

Note that in this case, `#[ext]` assigns a random name, so you cannot
import/export the generated trait.

### Visibility

There are two ways to specify visibility.

#### Impl-level visibility

The first way is to specify visibility at the impl level. For example:

```rust
use easy_ext::ext;

// unnamed
#[ext]
pub impl str {
    fn foo(&self) {}
}

// named
#[ext(StrExt)]
pub impl str {
    fn bar(&self) {}
}
```

#### Associated-item-level visibility

Another way is to specify visibility at the associated item level.

For example, if the method is `pub` then the trait will also be `pub`:

```rust
use easy_ext::ext;

#[ext(ResultExt)] // generate `pub trait ResultExt`
impl<T, E> Result<T, E> {
    pub fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

This is useful when migrate from an inherent impl to an extension trait.

Note that the visibility of all the associated items in the `impl` must be identical.

Note that you cannot specify impl-level visibility and associated-item-level visibility at the same time.

### [Supertraits](https://doc.rust-lang.org/reference/items/traits.html#supertraits)

If you want the extension trait to be a subtrait of another trait,
add `Self: SubTrait` bound to the `where` clause.

```rust
use easy_ext::ext;

#[ext(Ext)]
impl<T> T
where
    Self: Default,
{
    fn method(&self) {}
}
```

### Supported items

#### [Associated functions (methods)](https://doc.rust-lang.org/reference/items/associated-items.html#associated-functions-and-methods)

```rust
use easy_ext::ext;

#[ext]
impl<T> T {
    fn method(&self) {}
}
```

#### [Associated constants](https://doc.rust-lang.org/reference/items/associated-items.html#associated-constants)

```rust
use easy_ext::ext;

#[ext]
impl<T> T {
    const MSG: &'static str = "Hello!";
}
```

#### [Associated types](https://doc.rust-lang.org/reference/items/associated-items.html#associated-types)

```rust
use easy_ext::ext;

#[ext]
impl str {
    type Owned = String;

    fn method(&self) -> Self::Owned {
        self.to_owned()
    }
}
```

[rfc0445]: https://github.com/rust-lang/rfcs/blob/HEAD/text/0445-extension-trait-conventions.md

<!-- tidy:crate-doc:end -->
*/

#![doc(test(
    no_crate_inject,
    attr(
        deny(warnings, rust_2018_idioms, single_use_lifetimes),
        allow(dead_code, unused_variables)
    )
))]
#![forbid(unsafe_code)]

// older compilers require explicit `extern crate`.
#[allow(unused_extern_crates)]
extern crate proc_macro;

#[macro_use]
mod error;

mod ast;
mod iter;
mod to_tokens;

use std::{collections::hash_map::DefaultHasher, hash::Hasher, iter::FromIterator, mem};

use proc_macro::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

use crate::{
    ast::{
        parsing, printing::punct, Attribute, AttributeKind, FnArg, GenericParam, Generics,
        ImplItem, ItemImpl, ItemTrait, PredicateType, Signature, TraitItem, TraitItemConst,
        TraitItemMethod, TraitItemType, TypeParam, Visibility, WherePredicate,
    },
    error::{Error, Result},
    iter::TokenIter,
    to_tokens::ToTokens,
};

/// A lightweight attribute macro for easily writing [extension trait pattern][rfc0445].
///
/// See crate level documentation for details.
///
/// [rfc0445]: https://github.com/rust-lang/rfcs/blob/HEAD/text/0445-extension-trait-conventions.md
#[proc_macro_attribute]
pub fn ext(args: TokenStream, input: TokenStream) -> TokenStream {
    expand(args, input).unwrap_or_else(Error::into_compile_error)
}

fn expand(args: TokenStream, input: TokenStream) -> Result<TokenStream> {
    let trait_name = match parse_args(args)? {
        None => Ident::new(&format!("__ExtTrait{}", hash(&input)), Span::call_site()),
        Some(trait_name) => trait_name,
    };

    let mut item: ItemImpl = parsing::parse_impl(&mut TokenIter::new(input))?;

    let mut tokens = trait_from_impl(&mut item, trait_name)?.to_token_stream();
    tokens.extend(item.to_token_stream());
    Ok(tokens)
}

fn parse_args(input: TokenStream) -> Result<Option<Ident>> {
    let input = &mut TokenIter::new(input);
    let vis = ast::parsing::parse_visibility(input)?;
    if !vis.is_inherited() {
        bail!(vis, "use `{} impl` instead", vis);
    }
    let trait_name = input.parse_ident_opt();
    if !input.is_empty() {
        let tt = input.next().unwrap();
        bail!(tt, "unexpected token: `{}`", tt);
    }
    Ok(trait_name)
}

fn determine_trait_generics<'a>(
    generics: &mut Generics,
    self_ty: &'a [TokenTree],
) -> Option<&'a Ident> {
    if self_ty.len() != 1 {
        return None;
    }
    if let TokenTree::Ident(self_ty) = &self_ty[0] {
        let i = generics.params.iter().position(|(param, _)| {
            if let GenericParam::Type(param) = param {
                param.ident.to_string() == self_ty.to_string()
            } else {
                false
            }
        });
        if let Some(i) = i {
            let mut params = mem::replace(&mut generics.params, vec![]);
            let (param, _) = params.remove(i);
            generics.params = params;

            if let GenericParam::Type(TypeParam {
                colon_token: Some(colon_token), bounds, ..
            }) = param
            {
                let bounds = bounds.into_iter().filter(|(b, _)| !b.is_maybe).collect::<Vec<_>>();
                if !bounds.is_empty() {
                    let where_clause = generics.make_where_clause();
                    if let Some((_, p)) = where_clause.predicates.last_mut() {
                        p.get_or_insert_with(|| punct(',', Span::call_site()));
                    }
                    where_clause.predicates.push((
                        WherePredicate::Type(PredicateType {
                            lifetimes: None,
                            bounded_ty: vec![TokenTree::Ident(Ident::new("Self", self_ty.span()))]
                                .into_iter()
                                .collect(),
                            colon_token,
                            bounds,
                        }),
                        None,
                    ));
                }
            }

            return Some(self_ty);
        }
    }
    None
}

fn trait_from_impl(item: &mut ItemImpl, trait_name: Ident) -> Result<ItemTrait> {
    /// Replace `self_ty` with `Self`.
    struct ReplaceParam {
        self_ty: String,
        // Restrict the scope for removing `?Trait` bounds, because `?Trait`
        // bounds are only permitted at the point where a type parameter is
        // declared.
        remove_maybe: bool,
    }

    impl ReplaceParam {
        fn visit_token_stream(&self, tokens: &mut TokenStream) -> bool {
            let mut out: Vec<TokenTree> = vec![];
            let mut modified = false;
            let iter = tokens.clone().into_iter();
            for tt in iter {
                match tt {
                    TokenTree::Ident(ident) => {
                        if ident.to_string() == self.self_ty {
                            modified = true;
                            let self_ = Ident::new("Self", ident.span());
                            out.push(self_.into());
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
                *tokens = TokenStream::from_iter(out);
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
            for arg in &mut node.inputs {
                self.visit_fn_arg_mut(arg);
            }
            if let Some(ty) = &mut node.output {
                self.visit_token_stream(ty);
            }
        }

        fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
            match node {
                FnArg::Receiver(pat, _) => {
                    self.visit_token_stream(pat);
                }
                FnArg::Typed(pat, _, ty, _) => {
                    self.visit_token_stream(pat);
                    self.visit_token_stream(ty);
                }
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
            if let Some(where_clause) = &mut generics.where_clause {
                let predicates = Vec::with_capacity(where_clause.predicates.len());
                for (mut predicate, p) in mem::replace(&mut where_clause.predicates, predicates) {
                    match &mut predicate {
                        WherePredicate::Type(pred) => {
                            if self.remove_maybe {
                                let mut iter = pred.bounded_ty.clone().into_iter();
                                if let Some(TokenTree::Ident(i)) = iter.next() {
                                    if iter.next().is_none() && self.self_ty == i.to_string() {
                                        let bounds = mem::replace(&mut pred.bounds, vec![])
                                            .into_iter()
                                            .filter(|(b, _)| !b.is_maybe)
                                            .collect::<Vec<_>>();
                                        if !bounds.is_empty() {
                                            self.visit_token_stream(&mut pred.bounded_ty);
                                            pred.bounds = bounds;
                                            for (bound, _) in &mut pred.bounds {
                                                self.visit_token_stream(&mut bound.tokens);
                                            }
                                            where_clause.predicates.push((predicate, p));
                                        }
                                        continue;
                                    }
                                }
                            }

                            self.visit_token_stream(&mut pred.bounded_ty);
                            for (bound, _) in &mut pred.bounds {
                                self.visit_token_stream(&mut bound.tokens);
                            }
                        }
                        WherePredicate::Lifetime(_) => {}
                    }
                    where_clause.predicates.push((predicate, p));
                }
            }
        }
    }

    let mut generics = item.generics.clone();
    let mut visitor = determine_trait_generics(&mut generics, &item.self_ty)
        .map(|self_ty| ReplaceParam { self_ty: self_ty.to_string(), remove_maybe: false });

    if let Some(visitor) = &mut visitor {
        visitor.remove_maybe = true;
        visitor.visit_generics_mut(&mut generics);
        visitor.remove_maybe = false;
    }
    let ty_generics = generics.ty_generics();
    item.trait_ = Some((
        trait_name.clone(),
        ty_generics.to_token_stream(),
        Ident::new("for", Span::call_site()),
    ));

    // impl-level visibility
    let impl_vis = if item.vis.is_inherited() { None } else { Some(item.vis.clone()) };
    // assoc-item-level visibility
    let mut assoc_vis = None;
    let mut items = Vec::with_capacity(item.items.len());
    item.items.iter_mut().try_for_each(|item| {
        trait_item_from_impl_item(item, &mut assoc_vis, &impl_vis).map(|mut item| {
            if let Some(visitor) = &mut visitor {
                visitor.visit_trait_item_mut(&mut item);
            }
            items.push(item);
        })
    })?;

    let mut attrs = item.attrs.clone();
    find_remove(&mut item.attrs, AttributeKind::Doc); // https://github.com/taiki-e/easy-ext/issues/20
    attrs.push(Attribute::new(vec![
        TokenTree::Ident(Ident::new("allow", Span::call_site())),
        TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            std::iter::once(TokenTree::Ident(Ident::new(
                "patterns_in_fns_without_body",
                Span::call_site(),
            )))
            .collect(),
        )),
    ])); // mut self

    Ok(ItemTrait {
        attrs,
        // priority: impl-level visibility > assoc-item-level visibility > inherited visibility
        vis: impl_vis.unwrap_or_else(|| assoc_vis.unwrap_or(Visibility::Inherited)),
        unsafety: item.unsafety.clone(),
        trait_token: Ident::new("trait", item.impl_token.span()),
        ident: trait_name,
        generics,
        brace_token: item.brace_token,
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
            if current.is_inherited() {
                return Ok(());
            }
            bail!(current, "all associated items must have inherited visibility");
        }
        match prev {
            None => *prev = Some(current),
            Some(prev) if *prev == current => {}
            Some(prev) => {
                if prev.is_inherited() {
                    bail!(current, "all associated items must have inherited visibility");
                }
                bail!(
                    if current.is_inherited() { span } else { &current },
                    "all associated items must have a visibility of `{}`",
                    prev,
                );
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
                sig: {
                    let mut sig = impl_method.sig.clone();
                    for arg in &mut sig.inputs {
                        if let FnArg::Typed(pat, ..) = arg {
                            *pat = std::iter::once(TokenTree::Ident(Ident::new(
                                "_",
                                pat.clone().into_iter().next().unwrap().span(),
                            )))
                            .collect();
                        }
                    }
                    sig
                },
                semi_token: punct(';', impl_method.body.span()),
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
