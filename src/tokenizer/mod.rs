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

use regex::Regex;

#[derive(Debug)]
pub enum Block {
    Start,
    End,
}

#[derive(Debug)]
pub enum Token<'a> {
    Block(Block),
    TString(&'a str),
    Symbol(&'a str),
    Number(&'a str),
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
    let string = r#"(?:"(?P<string>(?:[^"\\\n]|\\.)*)")"#;
    let number = format!("(?:(?P<number>[0-9]+){})", delimer("number"));
    let block = r"(?P<block>\(|\))";
    let regex_str = format!(
        "^(?:{}|{}|{}|{}|(?P<whitespace>{}+))",
        number, string, symbol, block, whitespace
    );

    Regex::new(&regex_str).unwrap()
}

lazy_static! {
    static ref REGEX: Regex = gen_regex();
}

enum InternalToken<'a> {
    PublicToken(Token<'a>),
    Whitespace,
    EndOfFile(Option<Token<'a>>),
}

impl<'a> InternalToken<'a> {
    fn can_ignore(&self) -> bool {
        match self {
            InternalToken::PublicToken(_) => false,
            InternalToken::EndOfFile(_) => false,
            InternalToken::Whitespace => true,
        }
    }

    fn is_end_of_file(&self) -> bool {
        if let InternalToken::EndOfFile(_) = self {
            true
        } else {
            false
        }
    }

    fn to_option(self) -> Option<Token<'a>> {
        match self {
            InternalToken::PublicToken(token) => Some(token),
            _ => None,
        }
    }

    fn unwrap(self) -> Token<'a> {
        self.to_option().unwrap()
    }
}

trait ResultExt {
    fn is_end_of_file(&self) -> bool;
}

impl<'a, T> ResultExt for Result<InternalToken<'a>, T> {
    fn is_end_of_file(&self) -> bool {
        if let Ok(token) = self {
            token.is_end_of_file()
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub enum ErrorType {
    UnexpectedEndOfFile,
    UnknownToken,
}

#[derive(Debug)]
pub struct TokenizerError {
    e_type: ErrorType,
}

impl TokenizerError {
    fn unknown_token() -> Self {
        TokenizerError {
            e_type: ErrorType::UnknownToken,
        }
    }
}

pub struct Tokenizer<'a> {
    current_possition: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(token_stream: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            current_possition: token_stream,
        }
    }

    fn gen_token(&mut self) -> Result<InternalToken<'a>, TokenizerError> {
        fn handle_symbol_number<'a, T>(
            id: &'static str,
            captures: &regex::Captures<'a>,
            constructor: T,
        ) -> Option<(usize, InternalToken<'a>)>
        where
            T: Fn(&'a str) -> Token<'a>,
        {
            if let Some(token) = captures.name(id) {
                let ret = constructor(token.as_str());
                Some((
                    token.end(),
                    if captures.name(&format!("{}EndOfFile", id)).is_some() {
                        InternalToken::EndOfFile(Some(ret))
                    } else {
                        InternalToken::PublicToken(ret)
                    },
                ))
            } else {
                None
            }
        }

        if self.current_possition.is_empty() {
            return Ok(InternalToken::EndOfFile(None));
        }

        let unchecked_captures = REGEX.captures(self.current_possition);
        let captures = if let Some(cap) = unchecked_captures {
            cap
        } else {
            return Err(TokenizerError::unknown_token());
        };

        let mut end_of_token = captures.get(0).unwrap().end();

        let ret = if captures.name("whitespace").is_some() {
            InternalToken::Whitespace
        } else if let Some(r) =
            handle_symbol_number("symbol", &captures, |token| Token::Symbol(token))
        {
            end_of_token = r.0;
            r.1
        } else if let Some(r) =
            handle_symbol_number("number", &captures, |token| Token::Number(token))
        {
            end_of_token = r.0;
            r.1
        } else {
            InternalToken::PublicToken(if let Some(string) = captures.name("string") {
                Token::TString(string.as_str())
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

        self.current_possition = &self.current_possition[end_of_token..];

        Ok(ret)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Grab another token if its whitespace
        let mut token = self.gen_token();
        let mut can_ignore = true;
        while can_ignore {
            if let Ok(tok) = token {
                if tok.can_ignore() {
                    token = self.gen_token();
                    continue;
                } else {
                    token = Ok(tok);
                }
            }
            can_ignore = false;
        }

        if token.is_end_of_file() {
            if let Ok(InternalToken::EndOfFile(ret)) = token {
                ret.map(|inside| Ok(inside))
            } else {
                panic!()
            }
        } else {
            Some(token.map(|toke| toke.unwrap()))
        }
    }
}
