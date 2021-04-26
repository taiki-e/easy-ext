use std::iter;

use proc_macro2::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use syn::Lifetime;

pub(crate) trait ToTokens {
    fn to_tokens(&self, tokens: &mut TokenStream);

    fn to_token_stream(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens(&mut tokens);
        tokens
    }

    fn span(&self) -> Span {
        // https://github.com/dtolnay/quote/blob/1.0.9/src/spanned.rs
        let tokens = self.to_token_stream();
        let mut iter = tokens.into_iter().filter_map(|tt| {
            // FIXME: This shouldn't be required, since optimally spans should
            // never be invalid. This filter_map can probably be removed when
            // https://github.com/rust-lang/rust/issues/43081 is resolved.
            let span = tt.span();
            let debug = format!("{:?}", span);
            if debug.ends_with("bytes(0..0)") { None } else { Some(span) }
        });

        let first = match iter.next() {
            Some(span) => span,
            None => return Span::call_site(),
        };

        iter.fold(None, |_prev, next| Some(next)).and_then(|last| first.join(last)).unwrap_or(first)
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(iter::once(TokenTree::Ident(self.clone())));
    }
}

impl ToTokens for Punct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(iter::once(TokenTree::Punct(self.clone())));
    }
}

impl ToTokens for Literal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(iter::once(TokenTree::Literal(self.clone())));
    }
}

impl ToTokens for Group {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(iter::once(TokenTree::Group(self.clone())));
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

impl ToTokens for TokenTree {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(iter::once(self.clone()));
    }
}

impl ToTokens for TokenStream {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.clone());
    }
}

impl<T: ToTokens> ToTokens for Option<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(t) = self {
            T::to_tokens(t, tokens)
        }
    }
}

impl<T: ?Sized + ToTokens> ToTokens for &T {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        T::to_tokens(self, tokens)
    }
}

impl<T: ToTokens> ToTokens for [T] {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for t in self {
            T::to_tokens(t, tokens)
        }
    }
}

impl<T: ToTokens> ToTokens for [(T, Option<Punct>)] {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for (t, p) in self {
            T::to_tokens(t, tokens);
            p.to_tokens(tokens);
        }
    }
}
