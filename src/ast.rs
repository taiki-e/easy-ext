// Based on https://github.com/dtolnay/syn/blob/1.0.65/src/item.rs

use proc_macro2::{TokenStream, TokenTree};
use syn::{token, Attribute, Generics, Ident, Path, ReturnType, Token, Type, Visibility};

pub(crate) struct ItemImpl {
    pub(crate) attrs: Vec<Attribute>,
    defaultness: Option<Token![default]>,
    pub(crate) unsafety: Option<Token![unsafe]>,
    pub(crate) impl_token: Token![impl],
    pub(crate) generics: Generics,
    pub(crate) trait_: Option<(Path, Token![for])>,
    pub(crate) self_ty: Box<Type>,
    pub(crate) brace_token: token::Brace,
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
    defaultness: Option<Token![default]>,
    pub(crate) const_token: Token![const],
    pub(crate) ident: Ident,
    pub(crate) colon_token: Token![:],
    pub(crate) ty: Type,
    eq_token: Token![=],
    expr: Vec<TokenTree>,
    pub(crate) semi_token: Token![;],
}

pub(crate) struct ImplItemMethod {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    defaultness: Option<Token![default]>,
    pub(crate) sig: Signature,
    pub(crate) brace_token: token::Brace,
    body: TokenStream,
}

pub(crate) struct ImplItemType {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    defaultness: Option<Token![default]>,
    pub(crate) type_token: Token![type],
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    eq_token: Token![=],
    ty: Type,
    pub(crate) semi_token: Token![;],
}

#[derive(Clone)]
pub(crate) struct Signature {
    // [async|const] [unsafe] [extern [<abi>]] fn
    before_ident: Vec<TokenTree>,
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    paren_token: token::Paren,
    pub(crate) inputs: TokenStream,
    pub(crate) output: ReturnType,
}

pub(crate) struct ItemTrait {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) vis: Visibility,
    pub(crate) unsafety: Option<Token![unsafe]>,
    pub(crate) trait_token: Token![trait],
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) brace_token: token::Brace,
    pub(crate) items: Vec<TraitItem>,
}

pub(crate) enum TraitItem {
    Const(TraitItemConst),
    Method(TraitItemMethod),
    Type(TraitItemType),
}

pub(crate) struct TraitItemConst {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) const_token: Token![const],
    pub(crate) ident: Ident,
    pub(crate) colon_token: Token![:],
    pub(crate) ty: Type,
    pub(crate) semi_token: Token![;],
}

pub(crate) struct TraitItemMethod {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) sig: Signature,
    pub(crate) semi_token: Token![;],
}

pub(crate) struct TraitItemType {
    pub(crate) attrs: Vec<Attribute>,
    pub(crate) type_token: Token![type],
    pub(crate) ident: Ident,
    pub(crate) generics: Generics,
    pub(crate) semi_token: Token![;],
}

mod parsing {
    use proc_macro2::{Spacing, TokenTree};
    use syn::{
        braced, parenthesized,
        parse::{discouraged::Speculative, Parse, ParseStream},
        Abi, Attribute, Generics, Ident, Lifetime, Result, ReturnType, Token, Type, Visibility,
    };

    use super::{ImplItem, ImplItemConst, ImplItemMethod, ImplItemType, ItemImpl, Signature};

    impl Parse for ItemImpl {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let defaultness: Option<Token![default]> = input.parse()?;
            let unsafety: Option<Token![unsafe]> = input.parse()?;
            let impl_token: Token![impl] = input.parse()?;

            let has_generics = input.peek(Token![<])
                && (input.peek2(Token![>])
                    || input.peek2(Token![#])
                    || (input.peek2(Ident) || input.peek2(Lifetime))
                        && (input.peek3(Token![:])
                            || input.peek3(Token![,])
                            || input.peek3(Token![>]))
                    || input.peek2(Token![const]));
            let mut generics: Generics =
                if has_generics { input.parse()? } else { Generics::default() };

            let self_ty: Type = input.parse()?;

            generics.where_clause = input.parse()?;

            let content;
            let brace_token = braced!(content in input);

            let mut items = Vec::new();
            while !content.is_empty() {
                items.push(content.parse()?);
            }

            Ok(ItemImpl {
                attrs,
                defaultness,
                unsafety,
                impl_token,
                generics,
                trait_: None,
                self_ty: Box::new(self_ty),
                brace_token,
                items,
            })
        }
    }

    impl Parse for ImplItem {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let ahead = input.fork();
            let vis: Visibility = ahead.parse()?;

            let mut lookahead = ahead.lookahead1();
            let defaultness = if lookahead.peek(Token![default]) && !ahead.peek2(Token![!]) {
                let defaultness: Token![default] = ahead.parse()?;
                lookahead = ahead.lookahead1();
                Some(defaultness)
            } else {
                None
            };

            let mut item = if lookahead.peek(Token![fn]) || peek_signature(&ahead) {
                input.parse().map(ImplItem::Method)
            } else if lookahead.peek(Token![const]) {
                let const_token: Token![const] = ahead.parse()?;
                input.advance_to(&ahead);
                let ident: Ident = input.parse()?;
                let colon_token: Token![:] = input.parse()?;
                let ty: Type = input.parse()?;
                let eq_token = input.parse()?;

                let mut expr = vec![];
                let semi_token;
                loop {
                    match input.parse()? {
                        TokenTree::Punct(ref p)
                            if p.as_char() == ';' && p.spacing() == Spacing::Alone =>
                        {
                            semi_token = Token![;](p.span());
                            break;
                        }
                        tt => expr.push(tt),
                    }
                }

                return Ok(ImplItem::Const(ImplItemConst {
                    attrs,
                    vis,
                    defaultness,
                    const_token,
                    ident,
                    colon_token,
                    ty,
                    eq_token,
                    expr,
                    semi_token,
                }));
            } else if lookahead.peek(Token![type]) {
                input.parse().map(ImplItem::Type)
            } else {
                Err(lookahead.error())
            }?;

            {
                let item_attrs = match &mut item {
                    ImplItem::Const(item) => &mut item.attrs,
                    ImplItem::Method(item) => &mut item.attrs,
                    ImplItem::Type(item) => &mut item.attrs,
                };
                attrs.extend(item_attrs.drain(..));
                *item_attrs = attrs;
            }

            Ok(item)
        }
    }

    impl Parse for ImplItemMethod {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis: Visibility = input.parse()?;
            let defaultness: Option<Token![default]> = input.parse()?;
            let sig: Signature = input.parse()?;

            let content;
            let brace_token = braced!(content in input);
            let body = content.parse()?;

            Ok(ImplItemMethod { attrs, vis, defaultness, sig, brace_token, body })
        }
    }

    impl Parse for ImplItemType {
        fn parse(input: ParseStream<'_>) -> Result<Self> {
            Ok(ImplItemType {
                attrs: input.call(Attribute::parse_outer)?,
                vis: input.parse()?,
                defaultness: input.parse()?,
                type_token: input.parse()?,
                ident: input.parse()?,
                generics: {
                    let mut generics: Generics = input.parse()?;
                    generics.where_clause = input.parse()?;
                    generics
                },
                eq_token: input.parse()?,
                ty: input.parse()?,
                semi_token: input.parse()?,
            })
        }
    }

    fn peek_signature(input: ParseStream<'_>) -> bool {
        let fork = input.fork();
        fork.parse::<Option<Token![const]>>().is_ok()
            && fork.parse::<Option<Token![async]>>().is_ok()
            && fork.parse::<Option<Token![unsafe]>>().is_ok()
            && fork.parse::<Option<Abi>>().is_ok()
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

            let content;
            let paren_token = parenthesized!(content in input);
            let inputs = content.parse()?;

            let output: ReturnType = input.parse()?;
            generics.where_clause = input.parse()?;

            Ok(Signature { before_ident, ident, generics, paren_token, inputs, output })
        }
    }
}

mod printing {
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    use super::{
        ImplItem, ImplItemConst, ImplItemMethod, ImplItemType, ItemImpl, ItemTrait, Signature,
        TraitItem, TraitItemConst, TraitItemMethod, TraitItemType,
    };

    impl ToTokens for ItemTrait {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.trait_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.items);
            });
        }
    }

    impl ToTokens for ItemImpl {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.defaultness.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.impl_token.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            if let Some((path, for_token)) = &self.trait_ {
                path.to_tokens(tokens);
                for_token.to_tokens(tokens);
            }
            self.self_ty.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.items);
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
            tokens.append_all(&self.attrs);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItemMethod {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.sig.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItemType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
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
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            tokens.append_all(&self.expr);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItemMethod {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.sig.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.body.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ImplItemType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
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
            tokens.append_all(&self.before_ident);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.inputs.to_tokens(tokens);
            });
            self.output.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
        }
    }
}
