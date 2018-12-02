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
    let odd_symbol = r#"(?:[+-]|\.{3})"#;
    let special_subsequent = r#"[+.@-]"#;

    let mut inital = "(?:[[:alpha:]]|".to_string();
    inital.push_str(&special_inital);
    inital.push_str(")");

    let mut subsequent = "(?:[0-9]|".to_string();
    subsequent.push_str(&inital);
    subsequent.push_str("|");
    subsequent.push_str(special_subsequent);
    subsequent.push_str(")");

    let mut normal_symbol = "(?:".to_string();
    normal_symbol.push_str(&inital);
    normal_symbol.push_str(&subsequent);
    normal_symbol.push_str("*)");

    let mut symbol = "(?P<symbol>".to_string();
    symbol.push_str(&normal_symbol);
    symbol.push_str("|");
    symbol.push_str(&odd_symbol);
    symbol.push_str(")");

    let string = r#"(?:"(?P<string>(?:[^"\\\n]|\\.)*)")"#;

    let number = "(?P<number>[0-9]+)";

    let block = r#"(?P<block>\(|\))"#;

    let whitespace = r#"(?P<whiteSpace>[[:space:]]+)"#;

    let mut regex_str = "^(?:".to_string();
    regex_str.push_str(number);
    regex_str.push_str("|");
    regex_str.push_str(string);
    regex_str.push_str("|");
    regex_str.push_str(&symbol);
    regex_str.push_str("|");
    regex_str.push_str(block);
    regex_str.push_str("|");
    regex_str.push_str(whitespace);
    regex_str.push_str(")");

    Regex::new(&regex_str).unwrap()
}

lazy_static! {
    static ref REGEX: Regex = gen_regex();
}

pub struct Tokenizer<'a> {
    current_possition: &'a str,
}

#[derive(Eq, PartialEq)]
enum EofOrErr {
    EndOfFile,
    Error,
}

impl<'a> Tokenizer<'a> {
    pub fn new(token_stream: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            current_possition: token_stream,
        }
    }

    fn gen_token(&mut self) -> Result<Option<Token<'a>>, EofOrErr> {
        if self.current_possition.is_empty() {
            return Err(EofOrErr::EndOfFile);
        }

        let unchecked_captures = REGEX.captures(self.current_possition);
        let captures = if let Some(cap) = unchecked_captures {
            cap
        } else {
            return Err(EofOrErr::Error);
        };

        let ret = if captures.name("whiteSpace").is_some() {
            None
        } else {
            Some(if let Some(id) = captures.name("symbol") {
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

        Ok(ret)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut unchecked_token = self.gen_token();
        let token;
        loop {
            if let Ok(tok) = unchecked_token {
                if tok.is_some() {
                    token = tok.unwrap();
                    break;
                } else {
                    unchecked_token = self.gen_token()
                }
            } else if let Err(err) = unchecked_token {
                if err == EofOrErr::EndOfFile {
                    return None;
                } else {
                    return Some(Err(()));
                }
            }
        }

        Some(Ok(token))
    }
}
