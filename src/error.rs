// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::iter::{self, FromIterator};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::to_tokens::ToTokens;

macro_rules! format_err {
    ($span:expr, $msg:expr $(,)*) => {
        crate::error::Error::new(&$span, String::from($msg))
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

    // Based on https://github.com/dtolnay/syn/blob/2.0.95/src/error.rs#L282-L322
    pub(crate) fn into_compile_error(self) -> TokenStream {
        let (start, end) = (self.start_span, self.end_span);
        // Note that using builtin macros as {core,std}:: are only available on Rust 1.38+ https://github.com/rust-lang/rust/pull/63056
        // but that is fine because it only makes the diagnostics on the older version worse.
        // ::core::compile_error!($msg)
        TokenStream::from_iter(vec![
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Joint);
                punct.set_span(start);
                punct
            }),
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Alone);
                punct.set_span(start);
                punct
            }),
            TokenTree::Ident(Ident::new("core", start)),
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Joint);
                punct.set_span(start);
                punct
            }),
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Alone);
                punct.set_span(start);
                punct
            }),
            TokenTree::Ident(Ident::new("compile_error", start)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(start);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    iter::once(TokenTree::Literal({
                        let mut string = Literal::string(&self.msg);
                        string.set_span(end);
                        string
                    }))
                    .collect()
                });
                group.set_span(end);
                group
            }),
        ])
    }
}
