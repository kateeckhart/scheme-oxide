use tokenizer::{Tokenizer, TokenizerError, Token};
use types::*;

enum ParserToken {
    ParitalList {
        head: SchemePair,
        tail: SchemePair,
    },
    Datanum(SchemeType)
}

impl From<Token> for ParserToken {
    fn from(token: Token) -> ParserToken {
        unimplemented!()
    }
}

pub struct Parser<F> where F: std::io::Read {
    stack: Vec<ParserToken>,
    tokenizer: Tokenizer<F>,
}

pub enum ParserError {
    TokenizerError(TokenizerError),
}

impl From<TokenizerError> for ParserError {
    fn from(err: TokenizerError) -> ParserError {
        ParserError::TokenizerError(err)
    }
}

impl<F> Iterator for Parser<F> where F: std::io::Read {
    type Item = Result<SchemeType, ParserError>;

    fn next(&mut self) -> Option<Result<SchemeType, ParserError>> {
        loop {
            match self.stack.pop() {
                None => {
                    let token_or_none = self.tokenizer.next();
                    if let Some(Ok(token)) = token_or_none {
                        self.stack.push(token.into());
                    } else if let Some(Err(err)) = token_or_none {
                        return Some(Err(err.into()));
                    } else {
                        return None;
                    }
                }
                _ => unimplemented!()
            }
        }
    }
}
