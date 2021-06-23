// Based on:
// - https://github.com/dtolnay/syn/blob/1.0.70/src/item.rs
// - https://github.com/dtolnay/syn/blob/1.0.70/src/generics.rs

use std::fmt;

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

#[derive(Clone)]
pub(crate) struct Lifetime {
    pub(crate) apostrophe: Span,
    pub(crate) ident: Ident,
}

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

#[derive(Clone, Copy, PartialEq)]
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
    pub(crate) const_token: Option<Ident>,
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

pub(crate) mod parsing {
    use std::iter::FromIterator;

    use proc_macro::{Delimiter, Punct, Spacing, TokenStream, TokenTree};

    use super::{
        Attribute, AttributeKind, BoundLifetimes, ConstParam, GenericParam, Generics, ImplItem,
        ImplItemConst, ImplItemMethod, ImplItemType, ItemImpl, Lifetime, LifetimeDef,
        PredicateLifetime, PredicateType, Signature, TypeParam, TypeParamBound, Visibility,
        WhereClause, WherePredicate,
    };
    use crate::{error::Result, iter::TokenIter, to_tokens::ToTokens};

    fn parse_until_punct(input: &mut TokenIter, ch: char) -> Result<(Vec<TokenTree>, Punct)> {
        let mut buf = vec![];
        loop {
            let tt = input.next();
            match tt {
                Some(TokenTree::Punct(ref p))
                    if p.as_char() == ch && p.spacing() == Spacing::Alone =>
                {
                    if let Some(TokenTree::Punct(p)) = tt {
                        return Ok((buf, p));
                    }
                    unreachable!();
                }
                None => {
                    // TODO: pass scope span
                    return Err(error!(TokenStream::new(), "expected `{}`", ch));
                }
                Some(tt) => buf.push(tt),
            }
        }
    }

    fn append_tokens_until(
        input: &mut TokenIter,
        buf: &mut Vec<TokenTree>,
        visit_first_angle_bracket: bool,
        f: fn(Option<&TokenTree>) -> bool,
    ) -> Result<()> {
        let mut angle_bracket: i32 = 0 - (visit_first_angle_bracket as i32);
        loop {
            let tt = input.peek();
            match tt {
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
                            buf.push(input.next().unwrap());
                            continue;
                        }
                        _ => {}
                    }
                    angle_bracket -= 1;
                    if angle_bracket >= 0 {
                        buf.push(input.next().unwrap());
                        continue;
                    }
                }
                Some(_) | None => {}
            }
            if angle_bracket <= 0 && f(tt) {
                return Ok(());
            }
            buf.push(input.next().ok_or_else(|| {
                // TODO: pass scope span
                error!(TokenStream::new(), "unexpected end of input")
            })?);
        }
    }

    fn parse_attrs(input: &mut TokenIter) -> Result<Vec<Attribute>> {
        let mut attrs = Vec::new();
        while input.peek_t(&'#') {
            let pound_token = input.parse_punct('#')?;
            let tokens = input.parse_group(Delimiter::Bracket)?;
            let mut kind = AttributeKind::Other;
            let mut iter = TokenIter::new(tokens.stream());
            if let Some(TokenTree::Ident(i)) = iter.next() {
                match iter.next() {
                    // ignore #[path ...]
                    Some(TokenTree::Punct(ref p)) if p.as_char() == ':' => {}
                    _ => match &*i.to_string() {
                        "doc" => kind = AttributeKind::Doc,
                        "inline" => kind = AttributeKind::Inline,
                        _ => {}
                    },
                }
            }

            let attr = Attribute { pound_token, tokens, kind };
            attrs.push(attr);
        }
        Ok(attrs)
    }

    fn parse_generics(input: &mut TokenIter) -> Result<Generics> {
        if !input.peek_t(&'<') {
            return Ok(Generics::default());
        }

        let lt_token = input.parse_punct('<')?;

        let mut params = Vec::new();
        loop {
            if input.peek_t(&'>') {
                break;
            }

            let attrs = parse_attrs(input)?;
            let value = if input.peek_lifetime() {
                GenericParam::Lifetime(LifetimeDef { attrs, ..parse_lifetime_def(input)? })
            } else if input.peek_t(&"const") {
                GenericParam::Const(ConstParam { attrs, ..parse_const_param(input)? })
            } else if input.peek_t(&"_") {
                GenericParam::Type(TypeParam {
                    attrs,
                    ident: input.parse_ident()?,
                    colon_token: None,
                    bounds: Vec::new(),
                    eq_token: None,
                    default: None,
                })
            } else if input.peek_ident().is_some() {
                GenericParam::Type(TypeParam { attrs, ..parse_type_param(input)? })
            } else {
                return Err(error!(
                    input.next(),
                    "expected one of: lifetime, identifier, `const`, `_`"
                ));
            };

            if input.peek_t(&'>') {
                params.push((value, None));
                break;
            }
            let punct = input.parse_punct(',')?;
            params.push((value, Some(punct)));
        }

        let gt_token = input.parse_punct('>')?;

        Ok(Generics {
            lt_token: Some(lt_token),
            params,
            gt_token: Some(gt_token),
            where_clause: None,
        })
    }

    fn parse_lifetime(input: &mut TokenIter) -> Result<Lifetime> {
        let tt = input.next();
        match &tt {
            Some(TokenTree::Punct(p)) if p.as_char() == '\'' && p.spacing() == Spacing::Joint => {
                match input.next() {
                    Some(TokenTree::Ident(ident)) => Ok(Lifetime { apostrophe: p.span(), ident }),
                    Some(tt2) => Err(error!(
                        TokenStream::from_iter(vec![tt.unwrap(), tt2]),
                        "expected lifetime"
                    )),
                    None => Err(error!(p, "expected lifetime")),
                }
            }
            // TODO: pass scope span if tt is None
            tt => Err(error!(tt, "expected lifetime")),
        }
    }

    fn parse_lifetime_def(input: &mut TokenIter) -> Result<LifetimeDef> {
        let attrs = parse_attrs(input)?;
        let lifetime = parse_lifetime(input)?;
        let colon_token = input.parse_punct_opt(':');

        let mut bounds = TokenStream::new();
        if colon_token.is_some() {
            loop {
                if input.peek_t(&',') || input.peek_t(&'>') {
                    break;
                }
                let value = parse_lifetime(input)?;
                value.to_tokens(&mut bounds);
                if !input.peek_t(&'+') {
                    break;
                }
                let punct = input.parse_punct('+')?;
                punct.to_tokens(&mut bounds);
            }
        }

        Ok(LifetimeDef { attrs, lifetime, colon_token, bounds })
    }

    fn parse_bound_lifetimes(input: &mut TokenIter) -> Result<BoundLifetimes> {
        Ok(BoundLifetimes {
            for_token: input.parse_kw("for")?,
            lt_token: input.parse_punct('<')?,
            lifetimes: {
                let mut lifetimes = Vec::new();
                while !input.peek_t(&'>') {
                    let lifetime = parse_lifetime_def(input)?;
                    if input.peek_t(&'>') {
                        lifetimes.push((lifetime, None));
                        break;
                    }
                    let punct = input.parse_punct(',')?;
                    lifetimes.push((lifetime, Some(punct)));
                }
                lifetimes
            },
            gt_token: input.parse_punct('>')?,
        })
    }

    fn parse_type_param(input: &mut TokenIter) -> Result<TypeParam> {
        let attrs = parse_attrs(input)?;
        let ident = input.parse_ident()?;
        let colon_token = input.parse_punct_opt(':');

        let mut bounds = Vec::new();
        if colon_token.is_some() {
            loop {
                if input.peek_t(&',') || input.peek_t(&'>') || input.peek_t(&'=') {
                    break;
                }

                let is_maybe = input.peek_t(&'?') && !input.peek2_t(&"const");
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
                if !input.peek_t(&'+') {
                    bounds.push((TypeParamBound::new(value, is_maybe), None));
                    break;
                }
                let punct = input.parse_punct('+')?;
                bounds.push((TypeParamBound::new(value, is_maybe), Some(punct)));
            }
        }

        let mut default = None;
        let eq_token = input.parse_punct_opt('=');
        if eq_token.is_some() {
            default = Some({
                let mut ty = vec![];
                append_tokens_until(input, &mut ty, false, |next| match next {
                    Some(TokenTree::Punct(p)) if p.as_char() == '>' || p.as_char() == ',' => true,
                    None => true,
                    _ => false,
                })?;
                ty.into_iter().collect()
            });
        }

        Ok(TypeParam { attrs, ident, colon_token, bounds, eq_token, default })
    }

    fn const_argument(input: &mut TokenIter) -> Result<TokenTree> {
        let tt = input.next();
        match &tt {
            Some(TokenTree::Literal(_)) | Some(TokenTree::Ident(_)) => Ok(tt.unwrap()),
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => Ok(tt.unwrap()),
            // TODO: pass scope span if tt is None
            _ => Err(error!(tt, "expected one of: literal, ident, `{`")),
        }
    }

    fn parse_const_param(input: &mut TokenIter) -> Result<ConstParam> {
        let attrs = parse_attrs(input)?;
        let const_token = input.parse_kw("const")?;
        let ident = input.parse_ident()?;
        let colon_token = input.parse_punct(':')?;

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
        let eq_token = if input.peek_t(&'=') {
            let eq_token = input.parse_punct('=')?;
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

    fn parse_where_clause(input: &mut TokenIter) -> Result<WhereClause> {
        let where_token = input.parse_kw("where")?;
        let mut predicates = Vec::new();
        loop {
            if input.is_empty()
                || input.peek_t(&Delimiter::Brace)
                || input.peek_t(&',')
                || input.peek_t(&';')
                || input.peek_t(&':') && !input.peek2_t(&':')
                || input.peek_t(&'=')
            {
                break;
            }
            let value = parse_where_predicate(input)?;
            if !input.peek_t(&',') {
                predicates.push((value, None));
                break;
            }
            let punct = input.parse_punct(',')?;
            predicates.push((value, Some(punct)));
        }
        Ok(WhereClause { where_token, predicates })
    }

    fn parse_where_predicate(input: &mut TokenIter) -> Result<WherePredicate> {
        if input.peek_lifetime() && input.peek3_t(&':') {
            Ok(WherePredicate::Lifetime(PredicateLifetime {
                lifetime: parse_lifetime(input)?,
                colon_token: input.parse_punct(':')?,
                bounds: {
                    let mut bounds = Vec::new();
                    loop {
                        if input.is_empty()
                            || input.peek_t(&Delimiter::Brace)
                            || input.peek_t(&',')
                            || input.peek_t(&';')
                            || input.peek_t(&':')
                            || input.peek_t(&'=')
                        {
                            break;
                        }
                        let value = parse_lifetime(input)?;
                        if !input.peek_t(&'+') {
                            bounds.push((value, None));
                            break;
                        }
                        let punct = input.parse_punct('+')?;
                        bounds.push((value, Some(punct)));
                    }
                    bounds
                },
            }))
        } else {
            Ok(WherePredicate::Type(PredicateType {
                lifetimes: {
                    if input.peek_t(&"for") { Some(parse_bound_lifetimes(input)?) } else { None }
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
                colon_token: input.parse_punct(':')?,
                bounds: {
                    let mut bounds = Vec::new();
                    loop {
                        if input.is_empty()
                            || input.peek_t(&Delimiter::Brace)
                            || input.peek_t(&',')
                            || input.peek_t(&';')
                            || input.peek_t(&':') && !input.peek2_t(&':')
                            || input.peek_t(&'=')
                        {
                            break;
                        }

                        let is_maybe = input.peek_t(&'?') && !input.peek2_t(&"const");
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
                        if !input.peek_t(&'+') {
                            bounds.push((TypeParamBound::new(value, is_maybe), None));
                            break;
                        }
                        let punct = input.parse_punct('+')?;
                        bounds.push((TypeParamBound::new(value, is_maybe), Some(punct)));
                    }
                    bounds
                },
            }))
        }
    }

    pub(crate) fn parse_visibility(input: &mut TokenIter) -> Result<Visibility> {
        if input.peek_t(&"pub") {
            let pub_token = input.parse_ident()?;
            match input.peek() {
                Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis {} => {
                    let g = input.parse_group(Delimiter::Parenthesis)?;
                    Ok(Visibility::Restricted(pub_token, g))
                }
                _ => Ok(Visibility::Public(pub_token)),
            }
        } else if input.peek_t(&"crate") {
            if input.peek2_punct(':').map_or(false, |p| p.spacing() == Spacing::Joint)
                && input.peek3_t(&':')
            {
                Ok(Visibility::Inherited)
            } else {
                Ok(Visibility::Crate(input.parse_ident()?))
            }
        } else {
            Ok(Visibility::Inherited)
        }
    }

    pub(crate) fn parse_impl(input: &mut TokenIter) -> Result<ItemImpl> {
        let attrs = parse_attrs(input)?;
        let vis: Visibility = parse_visibility(input)?;
        let defaultness = input.parse_kw_opt("default");
        let unsafety = input.parse_kw_opt("unsafe");
        let impl_token = input.parse_kw("impl")?;

        let has_generics = input.peek_t(&'<')
            && (input.peek2_t(&'>')
                || input.peek2_t(&'#')
                || input.peek2_ident().is_some()
                    && (input.peek3_t(&':')
                        || input.peek3_t(&',')
                        || input.peek3_t(&'>')
                        || input.peek3_t(&'='))
                || input.peek2_lifetime()
                    && (input.peek4_t(&':')
                        || input.peek4_t(&',')
                        || input.peek4_t(&'>')
                        || input.peek4_t(&'='))
                || input.peek2_t(&"const"));
        let mut generics: Generics =
            if has_generics { parse_generics(input)? } else { Generics::default() };

        let const_token = input.parse_kw_opt("const");

        let mut self_ty = vec![];
        append_tokens_until(input, &mut self_ty, false, |next| match next {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => true,
            Some(TokenTree::Ident(i)) if i.to_string() == "where" => true,
            _ => false,
        })?;

        if input.peek_t(&"where") {
            generics.where_clause = Some(parse_where_clause(input)?);
        }

        let g = input.parse_group(Delimiter::Brace)?;
        let brace_token = g.span();
        let content = &mut TokenIter::new(g.stream());

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(parse_impl_item(content)?);
        }

        Ok(ItemImpl {
            attrs,
            vis,
            defaultness,
            unsafety,
            impl_token,
            generics,
            const_token,
            trait_: None,
            self_ty,
            brace_token,
            items,
        })
    }

    fn parse_impl_item(input: &mut TokenIter) -> Result<ImplItem> {
        let attrs = parse_attrs(input)?;
        let vis = parse_visibility(input)?;

        let defaultness = if input.peek_t(&"default") && !input.peek2_t(&'!') {
            Some(input.parse_kw("default")?)
        } else {
            None
        };

        if peek_signature(input) {
            let sig = parse_signature(input)?;
            let body = input.parse_group(Delimiter::Brace)?;
            Ok(ImplItem::Method(ImplItemMethod { attrs, vis, defaultness, sig, body }))
        } else if input.peek_t(&"const") {
            let const_token = input.parse_kw("const")?;
            let ident = input.parse_ident()?;
            let colon_token = input.parse_punct(':')?;

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
            let eq_token = input.parse_punct('=')?;

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
        } else if input.peek_t(&"type") {
            let type_token = input.parse_kw("type")?;
            let ident = input.parse_ident()?;
            let mut generics = parse_generics(input)?;

            if input.peek_t(&"where") {
                generics.where_clause = Some(parse_where_clause(input)?);
            }

            let eq_token = input.parse_punct('=')?;

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
            Err(error!(input.next(), "expected one of: `default`, `fn`, `const`, `type`"))
        }
    }

    fn peek_signature(input: &mut TokenIter) -> bool {
        let fork = &mut input.clone();
        fork.parse_kw_opt("const");
        fork.parse_kw_opt("async");
        fork.parse_kw_opt("unsafe");
        if fork.peek_t(&"extern") {
            fork.parse_kw("extern").ok();
            fork.parse_literal_opt();
        }
        fork.peek_t(&"fn")
    }

    fn parse_signature(input: &mut TokenIter) -> Result<Signature> {
        let mut before_ident = vec![];
        loop {
            let tt = input.tt()?;
            match &tt {
                TokenTree::Ident(i) if i.to_string() == "fn" => {
                    before_ident.push(tt);
                    break;
                }
                TokenTree::Group(g) if g.delimiter() == Delimiter::None => {
                    let mut iter = g.stream().into_iter();
                    if let Some(TokenTree::Ident(i)) = iter.next() {
                        if iter.next().is_none() && i.to_string() == "fn" {
                            before_ident.push(tt);
                            break;
                        }
                    }
                    before_ident.push(tt);
                }
                _ => before_ident.push(tt),
            }
        }

        let ident = input.parse_ident()?;
        let mut generics = parse_generics(input)?;

        let inputs = input.parse_group(Delimiter::Parenthesis)?;
        let inputs = Some(TokenTree::Group(inputs)).into_iter().collect();

        let output = if input.peek_punct('-').map_or(false, |p| p.spacing() == Spacing::Joint)
            && input.peek2_t(&'>')
        {
            let arrow1 = input.tt()?;
            let arrow2 = input.tt()?;
            let mut tokens = vec![arrow1, arrow2];
            append_tokens_until(input, &mut tokens, false, |next| match next {
                Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => true,
                Some(TokenTree::Ident(i)) if i.to_string() == "where" => true,
                None => true,
                _ => false,
            })?;
            Some(tokens.into_iter().collect())
        } else {
            None
        };

        if input.peek_t(&"where") {
            generics.where_clause = Some(parse_where_clause(input)?);
        }

        Ok(Signature { before_ident, ident, generics, inputs, output })
    }
}

pub(crate) mod printing {
    use proc_macro::{Delimiter, Group, Punct, Spacing, Span, TokenStream};

    use super::{
        Attribute, BoundLifetimes, ConstParam, GenericParam, Generics, ImplGenerics, ImplItem,
        ImplItemConst, ImplItemMethod, ImplItemType, ItemImpl, ItemTrait, Lifetime, LifetimeDef,
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

    impl ToTokens for Lifetime {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let mut apostrophe = Punct::new('\'', Spacing::Joint);
            apostrophe.set_span(self.apostrophe);
            apostrophe.to_tokens(tokens);
            self.ident.to_tokens(tokens);
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
                Visibility::Crate(i) | Visibility::Public(i) => i.to_tokens(tokens),
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

    fn brace(span: Span, tokens: &mut TokenStream, f: &dyn Fn(&mut TokenStream)) {
        let mut inner = TokenStream::new();
        f(&mut inner);
        let mut g = Group::new(Delimiter::Brace, inner);
        g.set_span(span);
        g.to_tokens(tokens);
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
            brace(self.brace_token, tokens, &|tokens| {
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
            self.const_token.to_tokens(tokens);
            if let Some((path, generics, for_)) = &self.trait_ {
                path.to_tokens(tokens);
                generics.to_tokens(tokens);
                for_.to_tokens(tokens);
            }
            self.self_ty.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            brace(self.brace_token, tokens, &|tokens| {
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
