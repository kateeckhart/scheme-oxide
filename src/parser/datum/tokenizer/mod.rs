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

use lazy_static::lazy_static;
use regex::Regex;
use std::io::{self, prelude::*};

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum Block {
    Start,
    End,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Block(Block),
    TString(String),
    Symbol(String),
    Number(String),
}

fn gen_regex() -> Regex {
    let special_inital = "[!$%&*/:<=>?^_~]";
    let odd_symbol = r"(?:[+-]|\.{3})";
    let whitespace = "(?:[[:space:]])";
    let delimer = |id| format!(r#"(?:{}|[()";]|(?P<{}EndOfFile>$))"#, whitespace, id);
    let special_subsequent = r"[+.@-]";
    let inital = format!("(?:[[:alpha:]]|{})", special_inital);
    let subsequent = format!("(?:[0-9]|{}|{})", inital, special_subsequent);
    let normal_symbol = format!("(?:{}{}*)", inital, subsequent);
    let symbol = format!(
        "(?:(?P<symbol>{}|{}){})",
        normal_symbol,
        odd_symbol,
        delimer("symbol")
    );
    let string_body = |id| format!(r#"(?P<{}Body>(?:[^"\\\n]|\\.)*)"#, id);
    let good_string = format!(r#"(?:"{}")"#, string_body("goodString"));
    let bad_eof_string = format!(r#"(?:"{}\\?$)"#, string_body("badEofString"));
    let number = format!("(?:(?P<number>[0-9]+){})", delimer("number"));
    let block = r"(?P<block>\(|\))";
    let clipped = r"(?P<clipped>(?:\.{2}|#)$)";
    let regex_str = format!(
        "^(?:{}|{}|{}|{}|(?P<whitespace>{}+)|{}|{})",
        number, symbol, good_string, block, whitespace, bad_eof_string, clipped
    );

    Regex::new(&regex_str).unwrap()
}

lazy_static! {
    static ref REGEX: Regex = gen_regex();
}

enum InternalToken {
    PublicToken(Token),
    Whitespace,
    EndOfFile(Option<Result<Token, TokenizerError>>),
}

impl InternalToken {
    fn can_ignore(&self) -> bool {
        match self {
            InternalToken::PublicToken(_) => false,
            InternalToken::EndOfFile(_) => false,
            InternalToken::Whitespace => true,
        }
    }

    fn into_option(self) -> Option<Token> {
        match self {
            InternalToken::PublicToken(token) => Some(token),
            _ => None,
        }
    }

    fn unwrap(self) -> Token {
        self.into_option().unwrap()
    }
}

#[derive(Debug)]
pub enum TokenizerError {
    TokenTooBig,
    IoError(io::Error),
    Utf8Error,
    UnexpectedEndOfFile,
    UnknownToken,
}

impl From<io::Error> for TokenizerError {
    fn from(err: io::Error) -> Self {
        TokenizerError::IoError(err)
    }
}

impl From<std::str::Utf8Error> for TokenizerError {
    fn from(_: std::str::Utf8Error) -> Self {
        TokenizerError::Utf8Error
    }
}

pub struct Tokenizer<F>
where
    F: Read,
{
    buffer: Vec<u8>,
    start: usize,
    last_codepoint: usize,
    end: usize,
    file: F,
}

const BUFFER_SIZE: usize = 1024 * 16;

impl<F> Tokenizer<F>
where
    F: Read,
{
    pub fn new(file: F) -> Self {
        Tokenizer {
            buffer: vec![0; BUFFER_SIZE],
            start: 0,
            last_codepoint: 0,
            end: 0,
            file,
        }
    }

    // True if end of file
    fn refill_buffer(&mut self) -> Result<bool, TokenizerError> {
        //Delete unused part of buffer.
        self.buffer.drain(0..self.start);
        let drained = self.start;
        //Make the buffer the same size again
        for _ in 0..drained {
            self.buffer.push(0)
        }
        self.start -= drained;
        self.end -= drained;
        self.last_codepoint -= drained;

        if self.end == self.buffer.len() {
            return Err(TokenizerError::TokenTooBig);
        }
        let old_end = self.end;
        let len = self.buffer.len();
        self.end += self.file.read(&mut self.buffer[old_end..len])?;
        if self.end == old_end {
            return Ok(true);
        }

        if let Err(utferr) = std::str::from_utf8(&self.buffer[self.start..self.end]) {
            self.last_codepoint = utferr.valid_up_to();
            if utferr.error_len().is_some() {
                return Err(TokenizerError::Utf8Error);
            }
        } else {
            self.last_codepoint = self.end;
        }

        Ok(false)
    }

    fn gen_token(&mut self) -> Result<InternalToken, TokenizerError> {
        fn handle_symbol_number<'a, T>(
            id: &'static str,
            captures: &regex::Captures<'a>,
            constructor: T,
        ) -> Option<(usize, InternalToken)>
        where
            T: Fn(String) -> Token,
        {
            if let Some(token) = captures.name(id) {
                let ret = constructor(token.as_str().to_string());
                Some((
                    token.end(),
                    if captures.name(&format!("{}EndOfFile", id)).is_some() {
                        InternalToken::EndOfFile(Some(Ok(ret)))
                    } else {
                        InternalToken::PublicToken(ret)
                    },
                ))
            } else {
                None
            }
        }

        if self.start == self.last_codepoint {
            self.refill_buffer()?;
            if self.start == self.last_codepoint {
                return Ok(InternalToken::EndOfFile(None));
            }
        }

        let current_possition = std::str::from_utf8(&self.buffer[self.start..self.last_codepoint])?;

        let unchecked_captures = REGEX.captures(current_possition);
        let captures = if let Some(cap) = unchecked_captures {
            cap
        } else {
            return Err(TokenizerError::UnknownToken);
        };

        let mut end_of_token = captures.get(0).unwrap().end();

        let ret = if captures.name("whitespace").is_some() {
            InternalToken::Whitespace
        } else if let Some(r) = handle_symbol_number("symbol", &captures, Token::Symbol) {
            end_of_token = r.0;
            r.1
        } else if let Some(r) = handle_symbol_number("number", &captures, Token::Number) {
            end_of_token = r.0;
            r.1
        } else if captures.name("badEofStringBody").is_some() {
            return Err(TokenizerError::UnexpectedEndOfFile);
        } else if captures.name("clipped").is_some() {
            return Ok(InternalToken::EndOfFile(Some(Err(
                TokenizerError::UnexpectedEndOfFile,
            ))));
        } else {
            InternalToken::PublicToken(if let Some(string) = captures.name("goodStringBody") {
                Token::TString(string.as_str().to_string())
            } else if let Some(block) = captures.name("block") {
                let block_char = block.as_str();
                if block_char == "(" {
                    Token::Block(Block::Start)
                } else if block_char == ")" {
                    Token::Block(Block::End)
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            })
        };

        if let InternalToken::EndOfFile(_) = ret {
        } else {
            self.start += end_of_token;
        }

        Ok(ret)
    }
}

impl<'a, F> Iterator for Tokenizer<F>
where
    F: Read,
{
    type Item = Result<Token, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Grab another token if its whitespace
        let mut unchecked_token = self.gen_token();
        let mut can_ignore = true;
        while can_ignore {
            if let Ok(token) = unchecked_token {
                if token.can_ignore() {
                    unchecked_token = self.gen_token();
                    continue;
                } else {
                    unchecked_token = Ok(token);
                }
            }
            can_ignore = false;
        }

        let is_eof = if let Ok(InternalToken::EndOfFile(_)) = unchecked_token {
            true
        } else if let Err(TokenizerError::UnexpectedEndOfFile) = unchecked_token {
            true
        } else {
            false
        };

        if is_eof {
            let status = self.refill_buffer();
            let end_of_file;
            if let Ok(eof) = status {
                end_of_file = eof;
            } else {
                return Some(Err(status.unwrap_err()));
            }

            if end_of_file {
                if self.last_codepoint != self.end {
                    return Some(Err(TokenizerError::Utf8Error));
                }

                // Clear buffer for eof
                self.start = 0;
                self.last_codepoint = 0;
                self.end = 0;
                if let Ok(eof_or_token) = unchecked_token {
                    if let InternalToken::EndOfFile(None) = eof_or_token {
                        None
                    } else if let InternalToken::EndOfFile(Some(token)) = eof_or_token {
                        Some(token)
                    } else {
                        Some(Ok(eof_or_token.unwrap()))
                    }
                } else if let Err(err) = unchecked_token {
                    Some(Err(err))
                } else {
                    unreachable!()
                }
            } else {
                self.next()
            }
        } else {
            Some(unchecked_token.map(InternalToken::unwrap))
        }
    }
}
