// Based on:
// - https://github.com/dtolnay/syn/blob/1.0.70/src/item.rs
// - https://github.com/dtolnay/syn/blob/1.0.70/src/generics.rs

use std::fmt;

use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use syn::Lifetime;

#[derive(Clone)]
pub(crate) struct Generics {
    pub(crate) lt_token: Option<Punct>,
    pub(crate) params: Vec<(GenericParam, Option<Punct>)>,
    pub(crate) gt_token: Option<Punct>,
    pub(crate) where_clause: Option<WhereClause>,
}

impl Generics {
    pub(crate) fn make_where_clause(&mut self) -> &mut WhereClause {
        self.where_clause.get_or_insert_with(|| WhereClause {
            where_token: Ident::new("where", Span::call_site()),
            predicates: Vec::new(),
        })
    }

    pub(crate) fn impl_generics(&self) -> ImplGenerics<'_> {
        ImplGenerics(self)
    }

    pub(crate) fn ty_generics(&self) -> TypeGenerics<'_> {
        TypeGenerics(self)
    }
}

impl Default for Generics {
    fn default() -> Self {
        Self { lt_token: None, params: Vec::new(), gt_token: None, where_clause: None }
    }
}

#[derive(Clone)]
pub(crate) enum GenericParam {
    /// A generic type parameter: `T: Into<String>`.
    Type(TypeParam),
    /// A lifetime definition: `'a: 'b + 'c + 'd`.
    Lifetime(LifetimeDef),
    /// A const generic parameter: `const LENGTH: usize`.
    Const(ConstParam),
}

#[derive(Clone)]
pub(crate) struct TypeParam {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) ident: Ident,
    pub(crate) colon_token: Option<Punct>,
    pub(crate) bounds: Vec<(TypeParamBound, Option<Punct>)>,
    pub(crate) eq_token: Option<Punct>,
    pub(crate) default: Option<TokenStream>,
}

#[derive(Clone)]
pub(crate) struct TypeParamBound {
    pub(crate) tokens: TokenStream,
    pub(crate) is_maybe: bool,
}

impl TypeParamBound {
    fn new(tokens: Vec<TokenTree>, is_maybe: bool) -> Self {
        Self { tokens: tokens.into_iter().collect(), is_maybe }
    }
}

#[derive(Clone)]
pub(crate) struct LifetimeDef {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) lifetime: Lifetime,
    pub(crate) colon_token: Option<Punct>,
    pub(crate) bounds: TokenStream,
}

#[derive(Clone)]
pub(crate) struct BoundLifetimes {
    pub(crate) for_token: Ident,
    pub(crate) lt_token: Punct,
    pub(crate) lifetimes: Vec<(LifetimeDef, Option<Punct>)>,
    pub(crate) gt_token: Punct,
}

#[derive(Clone)]
pub(crate) struct ConstParam {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) const_token: Ident,
    pub(crate) ident: Ident,
    pub(crate) colon_token: Punct,
    pub(crate) ty: TokenStream,
    pub(crate) eq_token: Option<Punct>,
    pub(crate) default: Option<TokenStream>,
}

pub(crate) struct ImplGenerics<'a>(&'a Generics);
pub(crate) struct TypeGenerics<'a>(&'a Generics);

#[derive(Clone)]
pub(crate) struct WhereClause {
    pub(crate) where_token: Ident,
    pub(crate) predicates: Vec<(WherePredicate, Option<Punct>)>,
}

#[derive(Clone)]
pub(crate) enum WherePredicate {
    Type(PredicateType),
    Lifetime(PredicateLifetime),
}

#[derive(Clone)]
pub(crate) struct PredicateType {
    pub(crate) lifetimes: Option<BoundLifetimes>,
    pub(crate) bounded_ty: TokenStream,
    pub(crate) colon_token: Punct,
    pub(crate) bounds: Vec<(TypeParamBound, Option<Punct>)>,
}

#[derive(Clone)]
pub(crate) struct PredicateLifetime {
    pub(crate) lifetime: Lifetime,
    pub(crate) colon_token: Punct,
    pub(crate) bounds: Vec<(Lifetime, Option<Punct>)>,
}

// Outer attribute
#[derive(Clone)]
pub(crate) struct Attribute {
    // `#`
    pub(crate) pound_token: Punct,
    // `[...]`
    pub(crate) tokens: Group,
    pub(crate) kind: AttributeKind,
}

#[derive(Clone, PartialEq)]
pub(crate) enum AttributeKind {
    // #[doc ...]
    Doc,
    // #[inline ...]
    Inline,
    Other,
}

impl Attribute {
    pub(crate) fn new(tokens: Vec<TokenTree>) -> Self {
        Self {
            pound_token: Punct::new('#', Spacing::Alone),
            tokens: Group::new(Delimiter::Bracket, tokens.into_iter().collect()),
            kind: AttributeKind::Other,
        }
    }
}

#[derive(Clone)]
pub(crate) enum Visibility {
    // `pub`.
    Public(Ident),
    // `crate`.
    Crate(Ident),
    //`pub(self)`, `pub(super)`, `pub(crate)`, or `pub(in some::module)`
    Restricted(Ident, Group),
    Inherited,
}

impl Visibility {
    pub(crate) fn is_inherited(&self) -> bool {
        match self {
            Visibility::Inherited => true,
            _ => false,
        }
    }
}

impl PartialEq for Visibility {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Visibility::Public(_), Visibility::Public(_))
            | (Visibility::Crate(_), Visibility::Crate(_))
            | (Visibility::Inherited, Visibility::Inherited) => true,
            (Visibility::Restricted(_, x), Visibility::Restricted(_, y)) => {
                x.stream().to_string() == y.stream().to_string()
            }
            _ => false,
        }
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Public(_) => f.write_str("pub"),
            Visibility::Crate(_) => f.write_str("crate"),
            Visibility::Inherited => Ok(()),
            Visibility::Restricted(_, g) => write!(f, "pub{}", g),
        }
    }
}

pub(crate) struct ItemImpl {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) defaultness: Option<Ident>,
    pub(crate) unsafety: Option<Ident>,
    pub(crate) impl_token: Ident,
    pub(crate) generics: Generics,
    pub(crate) trait_: Option<(Ident, TokenStream, Ident)>,
    pub(crate) self_ty: Vec<TokenTree>,
    pub(crate) brace_token: Span,
    pub(crate) items: Vec<ImplItem>,
}

pub(crate) enum ImplItem {
    Const(ImplItemConst),
    Method(ImplItemMethod),
    Type(ImplItemType),
}

pub(crate) struct ImplItemConst {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) defaultness: Option<Ident>,
    pub(crate) const_token: Ident,
    pub(crate) ident: Ident,
    pub(crate) colon_token: Punct,
    pub(crate) ty: TokenStream,
    pub(crate) eq_token: Punct,
    pub(crate) expr: Vec<TokenTree>,
    pub(crate) semi_token: Punct,
}

pub(crate) struct ImplItemMethod {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) defaultness: Option<Ident>,
    pub(crate) sig: Signature,
    pub(crate) body: Group,
}

pub(crate) struct ImplItemType {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) defaultness: Option<Ident>,
    pub(crate) type_token: Ident,
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) eq_token: Punct,
    pub(crate) ty: Vec<TokenTree>,
    pub(crate) semi_token: Punct,
}

#[derive(Clone)]
pub(crate) struct Signature {
    // [const] [async] [unsafe] [extern [<abi>]] fn
    pub(crate) before_ident: Vec<TokenTree>,
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) inputs: TokenStream,
    pub(crate) output: Option<TokenStream>,
}

pub(crate) struct ItemTrait {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) unsafety: Option<Ident>,
    pub(crate) trait_token: Ident,
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) brace_token: Span,
    pub(crate) items: Vec<TraitItem>,
}

pub(crate) enum TraitItem {
    Const(TraitItemConst),
    Method(TraitItemMethod),
    Type(TraitItemType),
}

pub(crate) struct TraitItemConst {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) const_token: Ident,
    pub(crate) ident: Ident,
    pub(crate) colon_token: Punct,
    pub(crate) ty: TokenStream,
    pub(crate) semi_token: Punct,
}

pub(crate) struct TraitItemMethod {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) sig: Signature,
    pub(crate) semi_token: Punct,
}

pub(crate) struct TraitItemType {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) type_token: Ident,
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) semi_token: Punct,
}

mod parsing {
    use proc_macro2::{Delimiter, Group, Literal, Punct, Spacing, TokenStream, TokenTree};
    use syn::{
        braced,
        parse::{Parse, ParseStream},
        token, Ident, Lifetime, Result, Token,
    };

    use super::{
        Attribute, AttributeKind, BoundLifetimes, ConstParam, GenericParam, Generics, ImplItem,
        ImplItemConst, ImplItemMethod, ImplItemType, ItemImpl, LifetimeDef, PredicateLifetime,
        PredicateType, Signature, TypeParam, TypeParamBound, Visibility, WhereClause,
        WherePredicate,
    };
    use crate::{iter::TokenIter, to_tokens::ToTokens};

    pub(crate) fn parse_group(input: ParseStream<'_>, delimiter: Delimiter) -> Result<Group> {
        let (ok, ch) = match delimiter {
            Delimiter::Brace => (input.peek(token::Brace), '{'),
            Delimiter::Bracket => (input.peek(token::Bracket), '['),
            Delimiter::Parenthesis => (input.peek(token::Paren), '('),
            _ => unreachable!(),
        };
        if !ok {
            return Err(input.error(format!("expected `{}`", ch)));
        }
        input.parse()
    }

    fn parse_punct(input: ParseStream<'_>, ch: char) -> Result<Punct> {
        let tt = input.parse()?;
        match tt {
            Some(TokenTree::Punct(ref p)) if p.as_char() == ch && p.spacing() == Spacing::Alone => {
                if let Some(TokenTree::Punct(p)) = tt {
                    Ok(p)
                } else {
                    unreachable!()
                }
            }
            Some(tt) => Err(error!(tt, "expected `{}`", ch)),
            None => Err(input.error(format!("expected `{}`", ch))),
        }
    }

    fn parse_punct_opt(input: ParseStream<'_>, ch: char) -> Result<Option<Punct>> {
        match input.fork().parse()? {
            Some(TokenTree::Punct(ref p)) if p.as_char() == ch && p.spacing() == Spacing::Alone => {
                if let TokenTree::Punct(p) = input.parse()? {
                    Ok(Some(p))
                } else {
                    unreachable!()
                }
            }
            Some(_) | None => Ok(None),
        }
    }

    fn parse_kw(input: ParseStream<'_>, kw: &str) -> Result<Ident> {
        let tt = input.parse()?;
        match &tt {
            Some(TokenTree::Ident(i)) if i == kw => {
                if let Some(TokenTree::Ident(i)) = tt {
                    Ok(i)
                } else {
                    unreachable!()
                }
            }
            Some(tt) => Err(error!(tt, "expected `{}`", kw)),
            None => Err(input.error(format!("expected `{}`", kw))),
        }
    }

    fn parse_kw_opt(input: ParseStream<'_>, kw: &str) -> Result<Option<Ident>> {
        match input.fork().parse()? {
            Some(TokenTree::Ident(ref i)) if i == kw => {
                if let TokenTree::Ident(i) = input.parse()? { Ok(Some(i)) } else { unreachable!() }
            }
            Some(_) | None => Ok(None),
        }
    }

    fn parse_ident(input: ParseStream<'_>) -> Result<Ident> {
        match input.parse()? {
            Some(TokenTree::Ident(i)) => Ok(i),
            Some(tt) => Err(error!(tt, "expected identifier")),
            None => Err(input.error("expected identifier")),
        }
    }

    fn parse_until_punct(input: ParseStream<'_>, ch: char) -> Result<(Vec<TokenTree>, Punct)> {
        let mut buf = vec![];
        loop {
            let tt = input.parse()?;
            match tt {
                Some(TokenTree::Punct(ref p))
                    if p.as_char() == ch && p.spacing() == Spacing::Alone =>
                {
                    if let Some(TokenTree::Punct(p)) = tt {
                        return Ok((buf, p));
                    } else {
                        unreachable!()
                    }
                }
                None => {
                    return Err(input.error(format!("expected `{}`", ch)));
                }
                Some(tt) => buf.push(tt),
            }
        }
    }

    fn append_tokens_until(
        input: ParseStream<'_>,
        buf: &mut Vec<TokenTree>,
        visit_first_angle_bracket: bool,
        mut f: impl FnMut(Option<&TokenTree>) -> bool,
    ) -> Result<()> {
        let mut angle_bracket: i32 = 0 - (visit_first_angle_bracket as i32);
        loop {
            let tt = input.fork().parse()?;
            match &tt {
                Some(TokenTree::Punct(p)) if p.as_char() == '<' => {
                    angle_bracket += 1;
                }
                Some(TokenTree::Punct(p)) if p.as_char() == '>' => {
                    match buf.last() {
                        Some(TokenTree::Punct(p))
                            if p.as_char() == '-' && p.spacing() == Spacing::Joint =>
                        {
                            // `->`
                            // It's so confusing with `>`, so do not visit it.
                            buf.push(input.parse::<TokenTree>()?);
                            continue;
                        }
                        _ => {}
                    }
                    angle_bracket -= 1;
                    if angle_bracket >= 0 {
                        buf.push(input.parse::<TokenTree>()?);
                        continue;
                    }
                }
                Some(_) | None => {}
            }
            if angle_bracket <= 0 && f(tt.as_ref()) {
                return Ok(());
            }
            buf.push(input.parse::<TokenTree>()?);
        }
    }

    fn parse_attrs(input: ParseStream<'_>) -> Result<Vec<Attribute>> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.call(|input| {
                let pound_token = input.parse()?;
                let tokens: Group = parse_group(input, Delimiter::Bracket)?;
                let mut kind = AttributeKind::Other;
                let mut iter = TokenIter::new(tokens.stream());
                if let Some(TokenTree::Ident(i)) = iter.next() {
                    match iter.next() {
                        // ignore #[path ...]
                        Some(TokenTree::Punct(ref p))
                            if p.as_char() == ':' && p.spacing() == Spacing::Joint => {}
                        _ => match &*i.to_string() {
                            "doc" => kind = AttributeKind::Doc,
                            "inline" => kind = AttributeKind::Inline,
                            _ => {}
                        },
                    }
                }

                Ok(Attribute { pound_token, tokens, kind })
            })?);
        }
        Ok(attrs)
    }

    impl Parse for Generics {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            if !input.peek(Token![<]) {
                return Ok(Generics::default());
            }

            let lt_token = parse_punct(input, '<')?;

            let mut params = Vec::new();
            loop {
                if input.peek(Token![>]) {
                    break;
                }

                let attrs = input.call(parse_attrs)?;
                let lookahead = input.lookahead1();
                let value = if lookahead.peek(Lifetime) {
                    GenericParam::Lifetime(LifetimeDef { attrs, ..input.parse()? })
                } else if lookahead.peek(Ident) {
                    GenericParam::Type(TypeParam { attrs, ..input.parse()? })
                } else if lookahead.peek(Token![const]) {
                    GenericParam::Const(ConstParam { attrs, ..input.parse()? })
                } else if input.peek(Token![_]) {
                    GenericParam::Type(TypeParam {
                        attrs,
                        ident: parse_ident(input)?,
                        colon_token: None,
                        bounds: Vec::new(),
                        eq_token: None,
                        default: None,
                    })
                } else {
                    return Err(lookahead.error());
                };

                if input.peek(Token![>]) {
                    params.push((value, None));
                    break;
                }
                let punct = parse_punct(input, ',')?;
                params.push((value, Some(punct)));
            }

            let gt_token = parse_punct(input, '>')?;

            Ok(Generics {
                lt_token: Some(lt_token),
                params,
                gt_token: Some(gt_token),
                where_clause: None,
            })
        }
    }

    impl Parse for LifetimeDef {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(parse_attrs)?;
            let lifetime = input.parse()?;
            let colon_token = parse_punct_opt(input, ':')?;

            let mut bounds = TokenStream::new();
            if colon_token.is_some() {
                loop {
                    if input.peek(Token![,]) || input.peek(Token![>]) {
                        break;
                    }
                    let value: Lifetime = input.parse()?;
                    value.to_tokens(&mut bounds);
                    if !input.peek(Token![+]) {
                        break;
                    }
                    let punct = parse_punct(input, '+')?;
                    punct.to_tokens(&mut bounds);
                }
            }

            Ok(LifetimeDef { attrs, lifetime, colon_token, bounds })
        }
    }

    impl Parse for BoundLifetimes {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            Ok(Self {
                for_token: parse_kw(input, "for")?,
                lt_token: parse_punct(input, '<')?,
                lifetimes: {
                    let mut lifetimes = Vec::new();
                    while !input.peek(Token![>]) {
                        let lifetime = input.parse()?;
                        if input.peek(Token![>]) {
                            lifetimes.push((lifetime, None));
                            break;
                        }
                        let punct = parse_punct(input, ',')?;
                        lifetimes.push((lifetime, Some(punct)));
                    }
                    lifetimes
                },
                gt_token: parse_punct(input, '>')?,
            })
        }
    }

    impl Parse for TypeParam {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(parse_attrs)?;
            let ident: Ident = input.parse()?;
            let colon_token = parse_punct_opt(input, ':')?;

            let mut bounds = Vec::new();
            if colon_token.is_some() {
                loop {
                    input.fork().parse::<TokenStream>().unwrap();
                    if input.peek(Token![,]) || input.peek(Token![>]) || input.peek(Token![=]) {
                        break;
                    }

                    let is_maybe = input.peek(Token![?]) && !input.peek2(Token![const]);
                    let mut value = vec![];
                    append_tokens_until(input, &mut value, false, |next| match next {
                        Some(TokenTree::Punct(p))
                            if p.as_char() == ','
                                || p.as_char() == '>'
                                || p.as_char() == '='
                                || p.as_char() == '+' =>
                        {
                            true
                        }
                        None => true,
                        _ => false,
                    })?;
                    if !input.peek(Token![+]) {
                        bounds.push((TypeParamBound::new(value, is_maybe), None));
                        break;
                    }
                    let punct = parse_punct(input, '+')?;
                    bounds.push((TypeParamBound::new(value, is_maybe), Some(punct)));
                }
            }

            input.fork().parse::<TokenStream>().unwrap();
            let mut default = None;
            let eq_token = parse_punct_opt(input, '=')?;
            if eq_token.is_some() {
                input.fork().parse::<TokenStream>().unwrap();
                default = Some({
                    let mut ty = vec![];
                    append_tokens_until(input, &mut ty, false, |next| match next {
                        Some(TokenTree::Punct(p)) if p.as_char() == '>' || p.as_char() == ',' => {
                            true
                        }
                        None => true,
                        _ => false,
                    })?;
                    ty.into_iter().collect()
                });
            }

            Ok(TypeParam { attrs, ident, colon_token, bounds, eq_token, default })
        }
    }

    fn const_argument(input: ParseStream<'_>) -> Result<TokenTree> {
        let tt = input.parse()?;
        match &tt {
            TokenTree::Literal(_) => Ok(tt),
            TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => Ok(tt),
            _ => Err(error!(tt, "expected literal or `{`")),
        }
    }

    impl Parse for ConstParam {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(parse_attrs)?;
            let const_token = parse_kw(input, "const")?;
            let ident = parse_ident(input)?;
            let colon_token = parse_punct(input, ':')?;

            let mut ty = vec![];
            append_tokens_until(input, &mut ty, false, |next| match next {
                Some(TokenTree::Punct(p))
                    if p.as_char() == '>'
                        || p.as_char() == '=' && p.spacing() == Spacing::Alone
                        || p.as_char() == ',' && p.spacing() == Spacing::Alone =>
                {
                    true
                }
                None => true,
                _ => false,
            })?;
            let mut default = None;
            let eq_token = if input.peek(Token![=]) {
                let eq_token = parse_punct(input, '=')?;
                default = Some(Some(const_argument(input)?).into_iter().collect());
                Some(eq_token)
            } else {
                None
            };

            Ok(ConstParam {
                attrs,
                const_token,
                ident,
                colon_token,
                ty: ty.into_iter().collect(),
                eq_token,
                default,
            })
        }
    }

    impl Parse for WhereClause {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let where_token = parse_kw(input, "where")?;
            let mut predicates = Vec::new();
            loop {
                if input.is_empty()
                    || input.peek(token::Brace)
                    || input.peek(Token![,])
                    || input.peek(Token![;])
                    || input.peek(Token![:]) && !input.peek(Token![::])
                    || input.peek(Token![=])
                {
                    break;
                }
                let value: WherePredicate = input.parse()?;
                if !input.peek(Token![,]) {
                    predicates.push((value, None));
                    break;
                }
                let punct = parse_punct(input, ',')?;
                predicates.push((value, Some(punct)));
            }
            Ok(Self { where_token, predicates })
        }
    }

    impl Parse for WherePredicate {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            if input.peek(Lifetime) && input.peek2(Token![:]) {
                Ok(WherePredicate::Lifetime(PredicateLifetime {
                    lifetime: input.parse()?,
                    colon_token: parse_punct(input, ':')?,
                    bounds: {
                        let mut bounds = Vec::new();
                        loop {
                            if input.is_empty()
                                || input.peek(token::Brace)
                                || input.peek(Token![,])
                                || input.peek(Token![;])
                                || input.peek(Token![:])
                                || input.peek(Token![=])
                            {
                                break;
                            }
                            let value: Lifetime = input.parse()?;
                            if !input.peek(Token![+]) {
                                bounds.push((value, None));
                                break;
                            }
                            let punct = parse_punct(input, '+')?;
                            bounds.push((value, Some(punct)));
                        }
                        bounds
                    },
                }))
            } else {
                Ok(WherePredicate::Type(PredicateType {
                    lifetimes: {
                        if input.peek(Token![for]) { Some(input.parse()?) } else { None }
                    },
                    bounded_ty: {
                        let mut ty = vec![];
                        append_tokens_until(input, &mut ty, false, |next| match next {
                            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => true,
                            Some(TokenTree::Punct(p))
                                if p.as_char() == ','
                                    || p.as_char() == '=' && p.spacing() == Spacing::Alone
                                    || p.as_char() == ':' && p.spacing() == Spacing::Alone =>
                            {
                                true
                            }
                            _ => false,
                        })?;
                        ty.into_iter().collect()
                    },
                    colon_token: parse_punct(input, ':')?,
                    bounds: {
                        let mut bounds = Vec::new();
                        loop {
                            if input.is_empty()
                                || input.peek(token::Brace)
                                || input.peek(Token![,])
                                || input.peek(Token![;])
                                || input.peek(Token![:]) && !input.peek(Token![::])
                                || input.peek(Token![=])
                            {
                                break;
                            }

                            let is_maybe = input.peek(Token![?]) && !input.peek2(Token![const]);
                            let mut value = vec![];
                            append_tokens_until(input, &mut value, false, |next| match next {
                                Some(TokenTree::Punct(p))
                                    if p.as_char() == ','
                                        || p.as_char() == '>'
                                        || p.as_char() == '='
                                        || p.as_char() == '+' =>
                                {
                                    true
                                }
                                None => true,
                                _ => false,
                            })?;
                            if !input.peek(Token![+]) {
                                bounds.push((TypeParamBound::new(value, is_maybe), None));
                                break;
                            }
                            let punct = parse_punct(input, '+')?;
                            bounds.push((TypeParamBound::new(value, is_maybe), Some(punct)));
                        }
                        bounds
                    },
                }))
            }
        }
    }

    impl Parse for Visibility {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            if input.peek(Token![pub]) {
                let pub_token = parse_kw(input, "pub")?;
                if input.peek(token::Paren) {
                    let g = parse_group(input, Delimiter::Parenthesis)?;
                    Ok(Visibility::Restricted(pub_token, g))
                } else {
                    Ok(Visibility::Public(pub_token))
                }
            } else if input.peek(Token![crate]) {
                if input.peek2(Token![::]) {
                    Ok(Visibility::Inherited)
                } else {
                    Ok(Visibility::Crate(parse_kw(input, "crate")?))
                }
            } else {
                Ok(Visibility::Inherited)
            }
        }
    }

    impl Parse for ItemImpl {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(parse_attrs)?;
            let vis: Visibility = input.parse()?;
            let defaultness = parse_kw_opt(input, "default")?;
            let unsafety = parse_kw_opt(input, "unsafe")?;
            let impl_token = parse_kw(&input, "impl")?;

            let has_generics = input.peek(Token![<])
                && (input.peek2(Token![>])
                    || input.peek2(Token![#])
                    || (input.peek2(Ident) || input.peek2(Lifetime))
                        && (input.peek3(Token![:])
                            || input.peek3(Token![,])
                            || input.peek3(Token![>])
                            || input.peek3(Token![=]))
                    || input.peek2(Token![const]));
            let mut generics: Generics =
                if has_generics { input.parse()? } else { Generics::default() };

            let mut self_ty = vec![];
            append_tokens_until(input, &mut self_ty, false, |next| match next {
                Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => true,
                Some(TokenTree::Ident(i)) if i == "where" => true,
                _ => false,
            })?;

            if input.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
            }

            let content;
            let brace_token = braced!(content in input);

            let mut items = Vec::new();
            while !content.is_empty() {
                items.push(content.parse()?);
            }

            Ok(ItemImpl {
                attrs,
                vis,
                defaultness,
                unsafety,
                impl_token,
                generics,
                trait_: None,
                self_ty,
                brace_token: brace_token.span,
                items,
            })
        }
    }

    impl Parse for ImplItem {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(parse_attrs)?;
            let vis: Visibility = input.parse()?;

            let mut lookahead = input.lookahead1();
            let defaultness = if lookahead.peek(Token![default]) && !input.peek2(Token![!]) {
                let defaultness = parse_kw(&input, "default")?;
                lookahead = input.lookahead1();
                Some(defaultness)
            } else {
                None
            };

            if lookahead.peek(Token![fn]) || peek_signature(&input) {
                let sig: Signature = input.parse()?;
                let body = parse_group(input, Delimiter::Brace)?;
                Ok(ImplItem::Method(ImplItemMethod { attrs, vis, defaultness, sig, body }))
            } else if lookahead.peek(Token![const]) {
                let const_token = parse_kw(input, "const")?;
                let ident: Ident = input.parse()?;
                let colon_token = parse_punct(input, ':')?;

                let mut ty = vec![];
                append_tokens_until(input, &mut ty, false, |next| match next {
                    Some(TokenTree::Punct(p))
                        if p.as_char() == '=' && p.spacing() == Spacing::Alone
                            || p.as_char() == ';' && p.spacing() == Spacing::Alone =>
                    {
                        true
                    }
                    _ => false,
                })?;
                let eq_token = parse_punct(input, '=')?;

                let (expr, semi_token) = parse_until_punct(input, ';')?;

                Ok(ImplItem::Const(ImplItemConst {
                    attrs,
                    vis,
                    defaultness,
                    const_token,
                    ident,
                    colon_token,
                    ty: ty.into_iter().collect(),
                    eq_token,
                    expr,
                    semi_token,
                }))
            } else if lookahead.peek(Token![type]) {
                let type_token = parse_kw(input, "type")?;
                let ident = input.parse()?;
                let mut generics: Generics = input.parse()?;

                if input.peek(Token![where]) {
                    generics.where_clause = Some(input.parse()?);
                }

                let eq_token = parse_punct(input, '=')?;

                let (ty, semi_token) = parse_until_punct(input, ';')?;

                Ok(ImplItem::Type(ImplItemType {
                    attrs,
                    vis,
                    defaultness,
                    type_token,
                    ident,
                    generics,
                    eq_token,
                    ty,
                    semi_token,
                }))
            } else {
                Err(lookahead.error())
            }
        }
    }

    fn peek_signature(input: ParseStream<'_>) -> bool {
        let fork = input.fork();
        fork.parse::<Option<Token![const]>>().is_ok()
            && fork.parse::<Option<Token![async]>>().is_ok()
            && fork.parse::<Option<Token![unsafe]>>().is_ok()
            && (if fork.peek(Token![extern]) {
                fork.parse::<Token![extern]>().is_ok() && fork.parse::<Option<Literal>>().is_ok()
            } else {
                true
            })
            && fork.peek(Token![fn])
    }

    impl Parse for Signature {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let mut before_ident = vec![];
            loop {
                let tt = input.parse()?;
                match &tt {
                    TokenTree::Ident(i) if i == "fn" => {
                        before_ident.push(tt);
                        break;
                    }
                    _ => before_ident.push(tt),
                }
            }

            let ident: Ident = input.parse()?;
            let mut generics: Generics = input.parse()?;

            let inputs = parse_group(input, Delimiter::Parenthesis)?;
            let inputs = Some(TokenTree::Group(inputs)).into_iter().collect();

            let output = if input.peek(Token![->]) {
                let arrow1 = input.parse()?;
                let arrow2 = input.parse()?;
                let mut tokens = vec![arrow1, arrow2];
                append_tokens_until(input, &mut tokens, false, |next| match next {
                    Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => true,
                    Some(TokenTree::Ident(i)) if i == "where" => true,
                    None => true,
                    _ => false,
                })?;
                Some(tokens.into_iter().collect())
            } else {
                None
            };

            if input.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
            }

            Ok(Signature { before_ident, ident, generics, inputs, output })
        }
    }
}

pub(crate) mod printing {
    use proc_macro2::{Delimiter, Group, Punct, Spacing, Span, TokenStream};

    use super::{
        Attribute, BoundLifetimes, ConstParam, GenericParam, Generics, ImplGenerics, ImplItem,
        ImplItemConst, ImplItemMethod, ImplItemType, ItemImpl, ItemTrait, LifetimeDef,
        PredicateLifetime, PredicateType, Signature, TraitItem, TraitItemConst, TraitItemMethod,
        TraitItemType, TypeGenerics, TypeParam, TypeParamBound, Visibility, WhereClause,
        WherePredicate,
    };
    use crate::to_tokens::ToTokens;

    pub(crate) fn punct(ch: char, span: Span) -> Punct {
        let mut p = Punct::new(ch, Spacing::Alone);
        p.set_span(span);
        p
    }

    fn tokens_or_default(p: &Option<Punct>, ch: char, tokens: &mut TokenStream) {
        match p {
            Some(p) => p.to_tokens(tokens),
            None => punct(ch, Span::call_site()).to_tokens(tokens),
        }
    }

    impl ToTokens for Generics {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            if self.params.is_empty() {
                return;
            }

            tokens_or_default(&self.lt_token, '<', tokens);

            // Print lifetimes before types and consts, regardless of their
            // order in self.params.
            //
            // TODO: ordering rules for const parameters vs type parameters have
            // not been settled yet. https://github.com/rust-lang/rust/issues/44580
            let mut trailing_or_empty = true;
            for (param, p) in &self.params {
                if let GenericParam::Lifetime(_) = param {
                    param.to_tokens(tokens);
                    p.to_tokens(tokens);
                    trailing_or_empty = p.is_some();
                }
            }
            for (param, p) in &self.params {
                match param {
                    GenericParam::Type(_) | GenericParam::Const(_) => {
                        if !trailing_or_empty {
                            punct(',', Span::call_site()).to_tokens(tokens);
                            trailing_or_empty = true;
                        }
                        param.to_tokens(tokens);
                        p.to_tokens(tokens);
                    }
                    GenericParam::Lifetime(_) => {}
                }
            }

            tokens_or_default(&self.gt_token, '>', tokens);
        }
    }

    impl ToTokens for GenericParam {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                GenericParam::Const(p) => p.to_tokens(tokens),
                GenericParam::Lifetime(l) => l.to_tokens(tokens),
                GenericParam::Type(t) => t.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for BoundLifetimes {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.for_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);
            self.lifetimes.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    impl ToTokens for LifetimeDef {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.lifetime.to_tokens(tokens);
            if !self.bounds.is_empty() {
                tokens_or_default(&self.colon_token, ':', tokens);
                self.bounds.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TypeParam {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            if !self.bounds.is_empty() {
                tokens_or_default(&self.colon_token, ':', tokens);
                for (bound, punct) in &self.bounds {
                    bound.to_tokens(tokens);
                    punct.to_tokens(tokens);
                }
            }
            if let Some(default) = &self.default {
                tokens_or_default(&self.eq_token, '=', tokens);
                default.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TypeParamBound {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.tokens.to_tokens(tokens);
        }
    }

    impl ToTokens for ConstParam {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            if let Some(default) = &self.default {
                tokens_or_default(&self.eq_token, '=', tokens);
                default.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for ImplGenerics<'_> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            if self.0.params.is_empty() {
                return;
            }

            tokens_or_default(&self.0.lt_token, '<', tokens);

            // Print lifetimes before types and consts, regardless of their
            // order in self.params.
            //
            // TODO: ordering rules for const parameters vs type parameters have
            // not been settled yet. https://github.com/rust-lang/rust/issues/44580
            let mut trailing_or_empty = true;
            for (param, p) in &self.0.params {
                if let GenericParam::Lifetime(_) = param {
                    param.to_tokens(tokens);
                    p.to_tokens(tokens);
                    trailing_or_empty = p.is_some();
                }
            }
            for (param, p) in &self.0.params {
                if let GenericParam::Lifetime(_) = param {
                    continue;
                }
                if !trailing_or_empty {
                    punct(',', Span::call_site()).to_tokens(tokens);
                    trailing_or_empty = true;
                }
                match param {
                    GenericParam::Lifetime(_) => unreachable!(),
                    GenericParam::Type(param) => {
                        // Leave off the type parameter defaults
                        param.attrs.to_tokens(tokens);
                        param.ident.to_tokens(tokens);
                        if !param.bounds.is_empty() {
                            tokens_or_default(&param.colon_token, ':', tokens);
                            param.bounds.to_tokens(tokens);
                        }
                    }
                    GenericParam::Const(param) => {
                        // Leave off the const parameter defaults
                        param.attrs.to_tokens(tokens);
                        param.const_token.to_tokens(tokens);
                        param.ident.to_tokens(tokens);
                        param.colon_token.to_tokens(tokens);
                        param.ty.to_tokens(tokens);
                    }
                }
                p.to_tokens(tokens);
            }

            tokens_or_default(&self.0.gt_token, '>', tokens);
        }
    }

    impl ToTokens for TypeGenerics<'_> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            if self.0.params.is_empty() {
                return;
            }

            tokens_or_default(&self.0.lt_token, '<', tokens);

            // Print lifetimes before types and consts, regardless of their
            // order in self.params.
            //
            // TODO: ordering rules for const parameters vs type parameters have
            // not been settled yet. https://github.com/rust-lang/rust/issues/44580
            let mut trailing_or_empty = true;
            for (param, p) in &self.0.params {
                if let GenericParam::Lifetime(def) = param {
                    // Leave off the lifetime bounds and attributes
                    def.lifetime.to_tokens(tokens);
                    p.to_tokens(tokens);
                    trailing_or_empty = p.is_some();
                }
            }
            for (param, p) in &self.0.params {
                if let GenericParam::Lifetime(_) = param {
                    continue;
                }
                if !trailing_or_empty {
                    punct(',', Span::call_site()).to_tokens(tokens);
                    trailing_or_empty = true;
                }
                match param {
                    GenericParam::Lifetime(_) => unreachable!(),
                    GenericParam::Type(param) => {
                        // Leave off the type parameter defaults
                        param.ident.to_tokens(tokens);
                    }
                    GenericParam::Const(param) => {
                        // Leave off the const parameter defaults
                        param.ident.to_tokens(tokens);
                    }
                }
                p.to_tokens(tokens);
            }

            tokens_or_default(&self.0.gt_token, '>', tokens);
        }
    }

    impl ToTokens for WhereClause {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            if !self.predicates.is_empty() {
                self.where_token.to_tokens(tokens);
                self.predicates.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for WherePredicate {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                WherePredicate::Lifetime(l) => l.to_tokens(tokens),
                WherePredicate::Type(t) => t.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for PredicateType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.lifetimes.to_tokens(tokens);
            self.bounded_ty.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for PredicateLifetime {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.lifetime.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for Visibility {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Visibility::Crate(i) => i.to_tokens(tokens),
                Visibility::Public(i) => i.to_tokens(tokens),
                Visibility::Restricted(i, g) => {
                    i.to_tokens(tokens);
                    g.to_tokens(tokens);
                }
                Visibility::Inherited => {}
            }
        }
    }

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.pound_token.to_tokens(tokens);
            self.tokens.to_tokens(tokens);
        }
    }

    fn delim(s: &str, span: Span, tokens: &mut TokenStream, f: impl FnOnce(&mut TokenStream)) {
        let delim = match s {
            "(" => Delimiter::Parenthesis,
            "[" => Delimiter::Bracket,
            "{" => Delimiter::Brace,
            " " => Delimiter::None,
            _ => panic!("unknown delimiter: {}", s),
        };
        let mut inner = TokenStream::new();
        f(&mut inner);
        let mut g = Group::new(delim, inner);
        g.set_span(span);
        g.to_tokens(tokens);
    }

    fn brace(span: Span, tokens: &mut TokenStream, f: impl FnOnce(&mut TokenStream)) {
        delim("{", span, tokens, f);
    }

    impl ToTokens for ItemTrait {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.vis.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.trait_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            brace(self.brace_token, tokens, |tokens| {
                self.items.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ItemImpl {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.impl_token.to_tokens(tokens);
            self.generics.impl_generics().to_tokens(tokens);
            if let Some((i, g, f)) = &self.trait_ {
                i.to_tokens(tokens);
                g.to_tokens(tokens);
                f.to_tokens(tokens);
            }
            self.self_ty.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            brace(self.brace_token, tokens, |tokens| {
                self.items.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TraitItem {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                TraitItem::Const(i) => i.to_tokens(tokens),
                TraitItem::Method(i) => i.to_tokens(tokens),
                TraitItem::Type(i) => i.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for TraitItemConst {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItemMethod {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.sig.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItemType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.type_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItem {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                ImplItem::Const(i) => i.to_tokens(tokens),
                ImplItem::Method(i) => i.to_tokens(tokens),
                ImplItem::Type(i) => i.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for ImplItemConst {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItemMethod {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.sig.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItemType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.attrs.to_tokens(tokens);
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.type_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for Signature {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.before_ident.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.inputs.to_tokens(tokens);
            self.output.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
        }
    }
}
