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
use crate::types::pair::ListFactory;
use crate::types::*;
use crate::{transpose_option, transpose_result};

enum ParserToken {
    PartialList(ListFactory),
    ListEnd,
    Datum(SchemeType),
}

impl ParserToken {
    fn from_token(token: Token) -> Result<ParserToken, DatumParserError> {
        Ok(match token {
            Token::Block(Block::Start) => ParserToken::PartialList(ListFactory::new()),
            Token::Block(Block::End) => ParserToken::ListEnd,
            Token::TString(string) => {
                ParserToken::Datum(SchemeType::String(unescape_string(&string)?))
            }
            Token::Symbol(symbol) => ParserToken::Datum(SchemeType::Symbol(symbol)),
            Token::Number(num) => {
                ParserToken::Datum(SchemeType::Number(i64::from_str_radix(&num, 10)?))
            }
        })
    }
}

#[derive(Debug)]
pub enum DatumParserError {
    TokenizerError(TokenizerError),
    NumberParse,
    Syntax,
    UnknownEscapeSequence,
}

impl From<TokenizerError> for DatumParserError {
    fn from(err: TokenizerError) -> DatumParserError {
        DatumParserError::TokenizerError(err)
    }
}

impl From<std::num::ParseIntError> for DatumParserError {
    fn from(_: std::num::ParseIntError) -> DatumParserError {
        DatumParserError::NumberParse
    }
}

fn unescape_string(string: &str) -> Result<String, DatumParserError> {
    let mut new_string = String::new();
    let mut iterator = string.chars();

    while let Some(charecter) = iterator.next() {
        if charecter == '\\' {
            let escape = iterator.next().unwrap();
            let escaped_char = match escape {
                '\\' => '\\',
                '"' => '"',
                _ => return Err(DatumParserError::UnknownEscapeSequence),
            };
            new_string.push(escaped_char);
        } else {
            new_string.push(charecter)
        }
    }
    new_string.shrink_to_fit();
    Ok(new_string)
}

pub struct DatumParser<F>
where
    F: std::io::Read,
{
    stack: Vec<ParserToken>,
    tokenizer: Tokenizer<F>,
}

impl<F> DatumParser<F>
where
    F: std::io::Read,
{
    pub fn new(file: F) -> Self {
        DatumParser {
            stack: Vec::new(),
            tokenizer: Tokenizer::new(file),
        }
    }

    //True if end of file
    fn push_input(&mut self) -> Result<bool, DatumParserError> {
        Ok(
            if let Some(token) = transpose_option(self.tokenizer.next())? {
                self.stack.push(ParserToken::from_token(token)?);
                false
            } else {
                true
            },
        )
    }

    fn iter_once(&mut self) -> Result<Option<SchemeType>, DatumParserError> {
        loop {
            let stack_top = self.stack.pop();
            match stack_top {
                None => {
                    if self.push_input()? {
                        return Ok(None);
                    }
                }
                Some(ParserToken::Datum(datum)) => match self.stack.pop() {
                    None => return Ok(Some(datum)),
                    Some(ParserToken::PartialList(mut factory)) => {
                        factory.push(datum);
                        self.stack.push(ParserToken::PartialList(factory))
                    }
                    _ => return Err(DatumParserError::Syntax),
                },
                Some(ParserToken::ListEnd) => match self.stack.pop() {
                    Some(ParserToken::PartialList(factory)) => {
                        let datum = factory.build();
                        self.stack.push(ParserToken::Datum(datum.into()));
                    }
                    _ => return Err(DatumParserError::Syntax),
                },
                _ => {
                    self.stack.push(stack_top.unwrap());
                    if self.push_input()? {
                        return Err(DatumParserError::TokenizerError(
                            TokenizerError::UnexpectedEndOfFile,
                        ));
                    }
                }
            }
        }
    }
}

impl<F> Iterator for DatumParser<F>
where
    F: std::io::Read,
{
    type Item = Result<SchemeType, DatumParserError>;

    fn next(&mut self) -> Option<Result<SchemeType, DatumParserError>> {
        transpose_result(self.iter_once())
    }
}
