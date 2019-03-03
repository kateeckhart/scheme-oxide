/*
    Copyright 2018 Alexander Eckhart

    This file is part of scheme-oxide.

    Scheme-oxide is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scheme-oxide is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with scheme-oxide.  If not, see <https://www.gnu.org/licenses/>.
*/

mod tokenizer;
use self::tokenizer::{Block, Token, Tokenizer, TokenizerError};
use crate::ast::{AstNode, AstSymbol, ListBuilder};
use crate::types::*;

enum ParserToken {
    PartialList(ListBuilder),
    ListEnd,
    Datum(AstNode),
    Dot,
}

impl ParserToken {
    fn from_token(token: Token) -> Result<ParserToken, ParserError> {
        Ok(match token {
            Token::Block(Block::Start) => ParserToken::PartialList(ListBuilder::new()),
            Token::Block(Block::End) => ParserToken::ListEnd,
            Token::TString(string) => {
                ParserToken::Datum(AstNode::from_string(unescape_string(&string)?))
            }
            Token::Symbol(symbol) => ParserToken::Datum(AstSymbol::new(&symbol).into()),
            Token::Number(num) => {
                ParserToken::Datum(AstNode::from_number(i64::from_str_radix(&num, 10)?))
            }
            Token::Bool(boolean) => ParserToken::Datum(AstNode::from_bool(boolean)),
            Token::Dot => ParserToken::Dot,
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
                _ => return Err(ParserError::UnknownEscapeSequence),
            };
            new_string.push(escaped_char);
        } else {
            new_string.push(charecter)
        }
    }
    new_string.shrink_to_fit();
    Ok(new_string)
}

pub struct Parser<'a> {
    stack: Vec<ParserToken>,
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            stack: Vec::new(),
            tokenizer: Tokenizer::new(input),
        }
    }

    //True if end of file
    fn push_input(&mut self) -> Result<bool, ParserError> {
        Ok(if let Some(token) = self.tokenizer.next().transpose()? {
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
                        return Ok(None);
                    }
                }
                Some(ParserToken::Datum(datum)) => match self.stack.pop() {
                    None => return Ok(Some(datum.to_datum())),
                    Some(ParserToken::PartialList(mut factory)) => {
                        factory.push(datum);
                        self.stack.push(ParserToken::PartialList(factory))
                    }
                    Some(ParserToken::Dot) => {
                        self.stack.push(ParserToken::Dot);

                        self.stack.push(ParserToken::Datum(datum));

                        if self.push_input()? {
                            return Err(ParserError::TokenizerError(
                                TokenizerError::UnexpectedEndOfFile,
                            ));
                        }
                    }
                    _ => return Err(ParserError::Syntax),
                },
                Some(ParserToken::ListEnd) => match self.stack.pop() {
                    Some(ParserToken::PartialList(factory)) => {
                        let datum = factory.build();
                        self.stack.push(ParserToken::Datum(datum.into()));
                    }
                    Some(ParserToken::Datum(rest)) => {
                        if let Some(ParserToken::Dot) = self.stack.pop() {
                        } else {
                            return Err(ParserError::Syntax);
                        }

                        let factory = if let Some(ParserToken::PartialList(fac)) = self.stack.pop()
                        {
                            fac
                        } else {
                            return Err(ParserError::Syntax);
                        };

                        let list = factory.build_with_tail(rest);

                        if list.is_empty_list() {
                            return Err(ParserError::Syntax);
                        } else {
                            self.stack.push(ParserToken::Datum(list.into()))
                        }
                    }
                    _ => return Err(ParserError::Syntax),
                },
                Some(ParserToken::Dot) => {
                    if let Some(ParserToken::PartialList(list)) = self.stack.pop() {
                        self.stack.push(ParserToken::PartialList(list))
                    } else {
                        return Err(ParserError::Syntax);
                    }

                    self.stack.push(ParserToken::Dot);

                    if self.push_input()? {
                        return Err(ParserError::TokenizerError(
                            TokenizerError::UnexpectedEndOfFile,
                        ));
                    }
                }
                _ => {
                    self.stack.push(stack_top.unwrap());
                    if self.push_input()? {
                        return Err(ParserError::TokenizerError(
                            TokenizerError::UnexpectedEndOfFile,
                        ));
                    }
                }
            }
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<SchemeType, ParserError>;

    fn next(&mut self) -> Option<Result<SchemeType, ParserError>> {
        self.iter_once().transpose()
    }
}
