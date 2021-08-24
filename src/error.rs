use std::iter::FromIterator;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::to_tokens::ToTokens;

macro_rules! format_err {
    ($span:expr, $msg:expr $(,)*) => {
        crate::Error::new(&$span, String::from($msg))
    };
    ($span:expr, $($tt:tt)*) => {
        format_err!($span, format!($($tt)*))
    };
}

macro_rules! bail {
    ($($tt:tt)*) => {
        return Err(format_err!($($tt)*))
    };
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) struct Error {
    start_span: Span,
    end_span: Span,
    msg: String,
}

impl Error {
    pub(crate) fn new(tokens: &dyn ToTokens, msg: String) -> Self {
        let mut iter = tokens.to_token_stream().into_iter();
        // `Span` on stable Rust has a limitation that only points to the first
        // token, not the whole tokens. We can work around this limitation by
        // using the first/last span of the tokens like `syn::Error::new_spanned` does.
        let start_span = iter.next().map_or_else(Span::call_site, |t| t.span());
        let end_span = iter.last().map_or(start_span, |t| t.span());

        Self { start_span, end_span, msg }
    }

    // Based on https://github.com/dtolnay/syn/blob/1.0.39/src/error.rs#L210-L237
    pub(crate) fn into_compile_error(self) -> TokenStream {
        // compile_error!($msg)
        TokenStream::from_iter(vec![
            TokenTree::Ident(Ident::new("compile_error", self.start_span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(self.start_span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter(vec![TokenTree::Literal({
                        let mut string = Literal::string(&self.msg);
                        string.set_span(self.end_span);
                        string
                    })])
                });
                group.set_span(self.end_span);
                group
            }),
        ])
    }
}
