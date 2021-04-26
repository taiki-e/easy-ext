// Based on https://github.com/dtolnay/proc-macro-hack/blob/0.5.19/src/quote.rs

use std::iter;

use proc_macro2::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use syn::Lifetime;

macro_rules! quote {
    () => {
        ::proc_macro2::TokenStream::new()
    };
    ($($tt:tt)*) => {{
        let mut tokens = ::proc_macro2::TokenStream::new();
        quote_each_token!(tokens $($tt)*);
        tokens
    }};
}

macro_rules! quote_each_token {
    ($tokens:ident # $var:ident $($rest:tt)*) => {
        $crate::quote::ToTokens::to_tokens(&$var, &mut $tokens);
        quote_each_token!($tokens $($rest)*);
    };
    ($tokens:ident $ident:ident $($rest:tt)*) => {
        <::proc_macro2::TokenStream as ::std::iter::Extend<_>>::extend(
            &mut $tokens,
            ::std::iter::once(
                ::proc_macro2::TokenTree::Ident(
                    ::proc_macro2::Ident::new(
                        stringify!($ident),
                        ::proc_macro2::Span::call_site(),
                    ),
                ),
            ),
        );
        quote_each_token!($tokens $($rest)*);
    };
    ($tokens:ident ( $($inner:tt)* ) $($rest:tt)*) => {
        <::proc_macro2::TokenStream as ::std::iter::Extend<_>>::extend(
            &mut $tokens,
            ::std::iter::once(
                ::proc_macro2::TokenTree::Group(
                    ::proc_macro2::Group::new(
                        ::proc_macro2::Delimiter::Parenthesis,
                        quote!($($inner)*),
                    ),
                ),
            ),
        );
        quote_each_token!($tokens $($rest)*);
    };
    ($tokens:ident [ $($inner:tt)* ] $($rest:tt)*) => {
        <::proc_macro2::TokenStream as ::std::iter::Extend<_>>::extend(
            &mut $tokens,
            ::std::iter::once(
                ::proc_macro2::TokenTree::Group(
                    ::proc_macro2::Group::new(
                        ::proc_macro2::Delimiter::Bracket,
                        quote!($($inner)*),
                    ),
                ),
            ),
        );
        quote_each_token!($tokens $($rest)*);
    };
    ($tokens:ident { $($inner:tt)* } $($rest:tt)*) => {
        <::proc_macro2::TokenStream as ::std::iter::Extend<_>>::extend(
            &mut $tokens,
            ::std::iter::once(
                ::proc_macro2::TokenTree::Group(
                    ::proc_macro2::Group::new(
                        ::proc_macro2::Delimiter::Brace,
                        quote!($($inner)*),
                    ),
                ),
            ),
        );
        quote_each_token!($tokens $($rest)*);
    };
    ($tokens:ident $punct:tt $($rest:tt)*) => {
        <::proc_macro2::TokenStream as ::std::iter::Extend<_>>::extend(
            &mut $tokens,
            stringify!($punct).parse::<::proc_macro2::TokenStream>(),
        );
        quote_each_token!($tokens $($rest)*);
    };
    ($tokens:ident) => {};
}

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
