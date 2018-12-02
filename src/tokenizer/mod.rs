/*
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
    Start(),
    End(),
}

#[derive(Debug)]
pub enum Token<'a> {
    Block(Block),
    TString(&'a str),
    Identifier(&'a str),
    Number(&'a str),
}

fn gen_regex() -> Regex {
    let special_inital = "[!$%&*/:<=>?^_~]";
    let odd_identifier = r#"(?:[+-]|\.{3})"#;
    let special_subsequent = r#"[+.@-]"#;

    let mut inital = "(?:[[:alpha:]]|".to_string();
    inital.push_str(&special_inital);
    inital.push_str(")");

    let mut subsequent = "(?:[0-9]|".to_string();
    subsequent.push_str(&inital); 
    subsequent.push_str("|"); 
    subsequent.push_str(special_subsequent); 
    subsequent.push_str(")");

    let mut normal_identifier = "(?:".to_string();
    normal_identifier.push_str(&inital);
    normal_identifier.push_str(&subsequent); 
    normal_identifier.push_str("*)");

    let mut identifier = "(?P<identifier>".to_string(); 
    identifier.push_str(&normal_identifier);
    identifier.push_str("|");
    identifier.push_str(&odd_identifier);
    identifier.push_str(")");

    let string = r#"(?:"(?P<string>(?:[^"\\\n]|\\.)*)")"#;

    let number = "(?P<number>[0-9]+)";

    let open_block = r#"(?P<openBlock>\()"#;
    let close_block = r#"(?P<closeBlock>\))"#;

    let mut block = "(?P<block>".to_string();
    block.push_str(&open_block); 
    block.push_str("|");
    block.push_str(&close_block);
    block.push_str(")");

    let whitespace = r#"(?P<whiteSpace>[[:space:]]+)"#;

    let mut regex_str = "^".to_string();
    regex_str.push_str(number);
    regex_str.push_str("|");
    regex_str.push_str(string);
    regex_str.push_str("|");
    regex_str.push_str(&identifier);
    regex_str.push_str("|");
    regex_str.push_str(&block);
    regex_str.push_str("|");
    regex_str.push_str(whitespace);

    Regex::new(&regex_str).unwrap()
}

lazy_static! {
    pub static ref REGEX: Regex = gen_regex();
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

        let ret;

        ret = if captures.name("whiteSpace").is_some() {
            None
        } else if let Some(id) = captures.name("identifier") {
            Some(Token::Identifier(id.as_str()))
        } else if let Some(number) = captures.name("number") {
            Some(Token::Number(number.as_str()))
        } else if let Some(string) = captures.name("string") {
            Some(Token::TString(string.as_str()))
        } else if captures.name("block").is_some() {
            if captures.name("openBlock").is_some() {
                Some(Token::Block(Block::Start()))
            } else if captures.name("closeBlock").is_some() {
                Some(Token::Block(Block::End()))
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
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
