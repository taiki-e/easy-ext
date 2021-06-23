use std::iter::FromIterator;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) struct Error {
    start_span: Span,
    end_span: Span,
    msg: String,
}

impl Error {
    pub(crate) fn new((start_span, end_span): (Span, Span), msg: String) -> Self {
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
