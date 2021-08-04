use proc_macro::{
    token_stream, Delimiter, Group, Ident, Literal, Punct, Spacing, TokenStream, TokenTree,
};

use crate::error::Result;

#[derive(Clone)]
pub(crate) struct TokenIter {
    stack: Vec<token_stream::IntoIter>,
    peeked: Option<TokenTree>,
    peeked2: Option<TokenTree>,
    peeked3: Option<TokenTree>,
    peeked4: Option<TokenTree>,
}

impl TokenIter {
    pub(crate) fn new(tokens: TokenStream) -> Self {
        Self {
            stack: vec![tokens.into_iter()],
            peeked: None,
            peeked2: None,
            peeked3: None,
            peeked4: None,
        }
    }

    pub(crate) fn is_empty(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn peek(&mut self) -> Option<&TokenTree> {
        self.peeked = self.next();
        self.peeked.as_ref()
    }

    pub(crate) fn peek_t(&mut self, token: &dyn Token) -> bool {
        match self.peek() {
            Some(tt) => token.match_token(tt),
            None => false,
        }
    }

    pub(crate) fn peek2(&mut self) -> Option<&TokenTree> {
        let peeked = self.next();
        let peeked2 = self.next();
        self.peeked = peeked;
        self.peeked2 = peeked2;
        self.peeked2.as_ref()
    }

    pub(crate) fn peek2_t(&mut self, token: &dyn Token) -> bool {
        match self.peek2() {
            Some(tt) => token.match_token(tt),
            None => false,
        }
    }

    pub(crate) fn peek3(&mut self) -> Option<&TokenTree> {
        let peeked = self.next();
        let peeked2 = self.next();
        let peeked3 = self.next();
        self.peeked = peeked;
        self.peeked2 = peeked2;
        self.peeked3 = peeked3;
        self.peeked3.as_ref()
    }

    pub(crate) fn peek3_t(&mut self, token: &dyn Token) -> bool {
        match self.peek3() {
            Some(tt) => token.match_token(tt),
            None => false,
        }
    }

    pub(crate) fn peek4(&mut self) -> Option<&TokenTree> {
        let peeked = self.next();
        let peeked2 = self.next();
        let peeked3 = self.next();
        let peeked4 = self.next();
        self.peeked = peeked;
        self.peeked2 = peeked2;
        self.peeked3 = peeked3;
        self.peeked4 = peeked4;
        self.peeked4.as_ref()
    }

    pub(crate) fn peek4_t(&mut self, token: &dyn Token) -> bool {
        match self.peek4() {
            Some(tt) => token.match_token(tt),
            None => false,
        }
    }

    pub(crate) fn peek_ident(&mut self) -> Option<&Ident> {
        match self.peek() {
            Some(TokenTree::Ident(i)) => Some(i),
            _ => None,
        }
    }

    pub(crate) fn peek2_ident(&mut self) -> Option<&Ident> {
        match self.peek2() {
            Some(TokenTree::Ident(i)) => Some(i),
            _ => None,
        }
    }

    pub(crate) fn peek3_ident(&mut self) -> Option<&Ident> {
        match self.peek3() {
            Some(TokenTree::Ident(i)) => Some(i),
            _ => None,
        }
    }

    pub(crate) fn parse_ident(&mut self) -> Result<Ident> {
        match self.next() {
            Some(TokenTree::Ident(i)) => Ok(i),
            // TODO: pass scope span if tt is None
            tt => bail!(tt, "expected identifier"),
        }
    }

    pub(crate) fn parse_ident_opt(&mut self) -> Option<Ident> {
        self.peek_ident()?;
        Some(self.parse_ident().unwrap())
    }

    pub(crate) fn parse_kw(&mut self, kw: &str) -> Result<Ident> {
        let tt = self.next();
        match &tt {
            Some(TokenTree::Ident(i)) if i.to_string() == kw => {
                if let Some(TokenTree::Ident(i)) = tt {
                    Ok(i)
                } else {
                    unreachable!()
                }
            }
            // TODO: pass scope span if tt is None
            tt => bail!(tt, "expected `{}`", kw),
        }
    }

    pub(crate) fn parse_kw_opt(&mut self, kw: &str) -> Option<Ident> {
        if self.peek_t(&kw) {
            Some(self.parse_ident().unwrap())
        } else {
            None
        }
    }

    pub(crate) fn peek_punct(&mut self, ch: char) -> Option<&Punct> {
        match self.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == ch => Some(p),
            _ => None,
        }
    }

    pub(crate) fn peek2_punct(&mut self, ch: char) -> Option<&Punct> {
        match self.peek2() {
            Some(TokenTree::Punct(p)) if p.as_char() == ch => Some(p),
            _ => None,
        }
    }

    pub(crate) fn parse_punct(&mut self, ch: char) -> Result<Punct> {
        let tt = self.next();
        match &tt {
            Some(TokenTree::Punct(p)) if p.as_char() == ch => {
                if let Some(TokenTree::Punct(p)) = tt {
                    Ok(p)
                } else {
                    unreachable!()
                }
            }
            // TODO: pass scope span if tt is None
            tt => bail!(tt, "expected `{}`", ch),
        }
    }

    pub(crate) fn parse_punct_opt(&mut self, ch: char) -> Option<Punct> {
        self.peek_punct(ch)?;
        Some(self.parse_punct(ch).unwrap())
    }

    pub(crate) fn peek_lifetime(&mut self) -> bool {
        self.peek_punct('\'').map_or(false, |p| p.spacing() == Spacing::Joint)
            && self.peek2_ident().is_some()
    }

    pub(crate) fn peek2_lifetime(&mut self) -> bool {
        self.peek2_punct('\'').map_or(false, |p| p.spacing() == Spacing::Joint)
            && self.peek3_ident().is_some()
    }

    pub(crate) fn parse_group(&mut self, delimiter: Delimiter) -> Result<Group> {
        let tt = self.next();
        match &tt {
            Some(TokenTree::Group(g)) if g.delimiter() == delimiter => {
                if let Some(TokenTree::Group(g)) = tt {
                    Ok(g)
                } else {
                    unreachable!()
                }
            }
            tt => {
                let d = match delimiter {
                    Delimiter::Brace => "`{`",
                    Delimiter::Bracket => "`[`",
                    Delimiter::Parenthesis => "`(`",
                    Delimiter::None => "none-delimited group",
                };
                // TODO: pass scope span if tt is None
                bail!(tt, "expected {}", d)
            }
        }
    }

    pub(crate) fn peek_literal(&mut self) -> Option<&Literal> {
        match self.peek() {
            Some(TokenTree::Literal(l)) => Some(l),
            _ => None,
        }
    }

    pub(crate) fn parse_literal(&mut self) -> Result<Literal> {
        match self.next() {
            Some(TokenTree::Literal(l)) => Ok(l),
            // TODO: pass scope span if tt is None
            tt => bail!(tt, "expected literal"),
        }
    }

    pub(crate) fn parse_literal_opt(&mut self) -> Option<Literal> {
        self.peek_literal()?;
        Some(self.parse_literal().unwrap())
    }

    pub(crate) fn tt(&mut self) -> Result<TokenTree> {
        self.next().ok_or_else(|| {
            // TODO: pass scope span
            format_err!(TokenStream::new(), "unexpected end of input")
        })
    }
}

// Based on https://github.com/dtolnay/proc-macro-hack/blob/0.5.18/src/iter.rs
impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tt) = self.peeked.take() {
            return Some(tt);
        }
        if let Some(tt) = self.peeked2.take() {
            return Some(tt);
        }
        if let Some(tt) = self.peeked3.take() {
            return Some(tt);
        }
        if let Some(tt) = self.peeked4.take() {
            return Some(tt);
        }
        loop {
            let top = self.stack.last_mut()?;
            match top.next() {
                None => drop(self.stack.pop()),
                Some(TokenTree::Group(ref group)) if group.delimiter() == Delimiter::None => {
                    self.stack.push(group.stream().into_iter());
                }
                Some(tt) => return Some(tt),
            }
        }
    }
}

pub(crate) trait Token {
    fn match_token(&self, tt: &TokenTree) -> bool;
}

impl Token for char {
    fn match_token(&self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Punct(p) => p.as_char() == *self,
            _ => false,
        }
    }
}

impl Token for &str {
    fn match_token(&self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Ident(i) => i.to_string() == *self,
            _ => false,
        }
    }
}

impl Token for Delimiter {
    fn match_token(&self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Group(g) => g.delimiter() == *self,
            _ => false,
        }
    }
}
