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
    let special_subsequent = r"[+.@-]";
    let inital = format!("(?:[[:alpha:]]|{})", special_inital);
    let subsequent = format!("(?:[0-9]|{}|{})", inital, special_subsequent);
    let normal_symbol = format!("(?:{}{}*)", inital, subsequent);
    let symbol = format!("(?P<symbol>{}|{})", normal_symbol, odd_symbol);
    let string = r#"(?:"(?P<string>(?:[^"\\\n]|\\.)*)")"#;
    let number = "(?P<number>[0-9]+)";
    let block = r"(?P<block>\(|\))";
    let whitespace = "(?P<whitespace>[[:space:]]+)";
    let regex_str = format!(
        "^(?:{}|{}|{}|{}|{})",
        number, string, symbol, block, whitespace
    );

    Regex::new(&regex_str).unwrap()
}

lazy_static! {
    static ref REGEX: Regex = gen_regex();
}

pub struct Tokenizer<'a> {
    current_possition: &'a str,
}

enum TokenOrWhitespace<'a> {
    Token(Token<'a>),
    Whitespace,
}

impl<'a> TokenOrWhitespace<'a> {
    fn is_whitespace(&self) -> bool {
        match self {
            TokenOrWhitespace::Token(_) => false,
            TokenOrWhitespace::Whitespace => true,
        }
    }

    fn to_option(self) -> Option<Token<'a>> {
        match self {
            TokenOrWhitespace::Token(token) => Some(token),
            TokenOrWhitespace::Whitespace => None,
        }
    }

    fn unwrap(self) -> Token<'a> {
        self.to_option().unwrap()
    }
}

#[derive(Debug)]
pub struct TokenizerError;

impl<'a> Tokenizer<'a> {
    pub fn new(token_stream: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            current_possition: token_stream,
        }
    }

    fn gen_token(&mut self) -> Option<Result<TokenOrWhitespace<'a>, TokenizerError>> {
        if self.current_possition.is_empty() {
            return None;
        }

        let unchecked_captures = REGEX.captures(self.current_possition);
        let captures = if let Some(cap) = unchecked_captures {
            cap
        } else {
            return Some(Err(TokenizerError));
        };

        let ret = if captures.name("whitespace").is_some() {
            TokenOrWhitespace::Whitespace
        } else {
            TokenOrWhitespace::Token(if let Some(id) = captures.name("symbol") {
                Token::Symbol(id.as_str())
            } else if let Some(number) = captures.name("number") {
                Token::Number(number.as_str())
            } else if let Some(string) = captures.name("string") {
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

        // Advance the position by the amount matched
        let end_of_token = captures.get(0).unwrap().end();

        self.current_possition = self.current_possition.split_at(end_of_token).1;

        Some(Ok(ret))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Grab another token if its whitespace
        let mut token = self.gen_token();
        let mut is_whitespace = true;
        while is_whitespace {
            if let Some(token_or_err) = token {
                if let Ok(tok) = token_or_err {
                    if tok.is_whitespace() {
                        token = self.gen_token();
                        continue;
                    } else {
                        token = Some(Ok(tok));
                    }
                } else {
                    token = Some(token_or_err);
                }
            }
            is_whitespace = false;
        }

        token.map(|toke| toke.map(|tok| tok.unwrap()))
    }
}
