// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::iter;

use proc_macro::{Group, Ident, Punct, TokenStream, TokenTree};

pub(crate) trait ToTokens {
    fn to_tokens(&self, tokens: &mut TokenStream);

    fn to_token_stream(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens(&mut tokens);
        tokens
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

impl ToTokens for Group {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(iter::once(TokenTree::Group(self.clone())));
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
            T::to_tokens(t, tokens);
        }
    }
}

impl<T: ?Sized + ToTokens> ToTokens for &T {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        T::to_tokens(self, tokens);
    }
}

impl<T: ToTokens> ToTokens for [T] {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for t in self {
            T::to_tokens(t, tokens);
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
