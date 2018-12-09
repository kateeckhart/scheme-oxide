use tokenizer::{Tokenizer, TokenizerError, Token, Block};
use {transpose_result, transpose_option};
use types::*;

enum ParserToken {
    PartialList {
        head: SchemePair,
        tail: SchemePair,
    },
    ListBegin,
    ListEnd,
    Datum(SchemeType),
}

impl ParserToken {
    fn from_token(token: Token) -> Result<ParserToken, ParserError> {
        Ok(match token {
            Token::Block(Block::Start) =>
                ParserToken::ListBegin,
            Token::Block(Block::End) =>
                ParserToken::ListEnd,
            Token::TString(string) => 
                ParserToken::Datum(SchemeType::String(unescape_string(&string)?)),
            Token::Symbol(symbol) => 
                ParserToken::Datum(SchemeType::Symbol(symbol)),
            Token::Number(num) =>
                ParserToken::Datum(SchemeType::Number(i64::from_str_radix(&num, 10)?))
        })
    }
}

#[derive(Debug)]
pub enum ParserError {
    TokenizerError(TokenizerError),
    NumberParse,
    Syntax,
    UnknownEscapeSequence,
}

impl From<TokenizerError> for ParserError {
    fn from(err: TokenizerError) -> ParserError {
        ParserError::TokenizerError(err)
    }
}

impl From<std::num::ParseIntError> for ParserError {
    fn from(_: std::num::ParseIntError) -> ParserError {
        ParserError::NumberParse
    }
}

fn unescape_string(string: &str) -> Result<String, ParserError> {
    let mut new_string = String::new();
    let mut iterator = string.chars();
    
    while let Some(charecter) = iterator.next() {
        if charecter == '\\' {
            let escape = iterator.next().unwrap();
            let escaped_char = match escape {
                '\\' => '\\',
                '"' => '"',
                _ => return Err(ParserError::UnknownEscapeSequence)
            };
            new_string.push(escaped_char);
        } else {
            new_string.push(charecter)
        }
    }
    new_string.shrink_to_fit();
    Ok(new_string)
}

pub struct Parser<F> where F: std::io::Read {
    stack: Vec<ParserToken>,
    tokenizer: Tokenizer<F>,
}

impl<F> Parser<F> where F: std::io::Read {
    pub fn new(file: F) -> Self {
        Parser {
            stack: Vec::new(),
            tokenizer: Tokenizer::new(file),
        }
    }

    //True if end of file
    fn push_input(&mut self) -> Result<bool, ParserError> {
        Ok(if let Some(token) = transpose_option(self.tokenizer.next())? {
            self.stack.push(ParserToken::from_token(token)?);
            false
        } else {
            true
        })
    }

    fn iter_once(&mut self) -> Result<Option<SchemeType>, ParserError> {
        loop {
            let stack_top = self.stack.pop();
            match stack_top {
                None => {
                    if self.push_input()? {
                        return Ok(None)
                    }
                }
                Some(ParserToken::Datum(datum)) => {
                    match self.stack.pop() {
                        None => {
                            return Ok(Some(datum))
                        },
                        Some(ParserToken::ListBegin) => {
                            let list = SchemePair::new(datum, SchemeType::EmptyList);
                            self.stack.push(ParserToken::PartialList {
                                head: list.clone(),
                                tail: list,
                            })
                        },
                        Some(ParserToken::PartialList {head, tail}) => {
                            let new_tail = SchemePair::new(datum, SchemeType::EmptyList);
                            tail.set_cdr(SchemeType::Pair(new_tail.clone()));
                            self.stack.push(ParserToken::PartialList {
                                head,
                                tail: new_tail,
                            })
                        },
                        _ => return Err(ParserError::Syntax)
                    }
                },
                Some(ParserToken::ListEnd) => {
                    match self.stack.pop() {
                        Some(ParserToken::ListBegin) => {
                            self.stack.push(ParserToken::Datum(SchemeType::EmptyList))
                        },
                        Some(ParserToken::PartialList {head, ..}) => {
                            self.stack.push(ParserToken::Datum(SchemeType::Pair(head)))
                        },
                        _ => return Err(ParserError::Syntax)
                    }
                },
                _ => {
                    self.stack.push(stack_top.unwrap());
                    if self.push_input()? {
                        return Err(ParserError::TokenizerError(TokenizerError::UnexpectedEndOfFile))
                    }
                }
            }
        }
    }
}

impl<F> Iterator for Parser<F> where F: std::io::Read {
    type Item = Result<SchemeType, ParserError>;

    fn next(&mut self) -> Option<Result<SchemeType, ParserError>> {
        transpose_result(self.iter_once())
    }
}
