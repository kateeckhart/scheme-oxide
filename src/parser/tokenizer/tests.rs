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

use super::{Block, Token, Tokenizer, TokenizerError, BUFFER_SIZE};
use std::io::Cursor;

fn cutoff_string(long_string: &str) {
    let mut tokenizer = Tokenizer::new(Cursor::new(format!(
        r#"("{}""{}""#,
        long_string, long_string
    )));
    assert_eq!(
        tokenizer.next().unwrap().unwrap(),
        Token::Block(Block::Start)
    );
    for _ in 0..2 {
        let token = tokenizer.next().unwrap().unwrap();
        assert_eq!(token, Token::TString(long_string.to_string()));
    }
    assert!(tokenizer.next().is_none())
}

#[test]
fn cutoff_normal_string() {
    let mut long_string = String::new();
    for _ in 0..BUFFER_SIZE - 2 {
        long_string.push('a');
    }
    cutoff_string(&long_string)
}

#[test]
fn cutoff_string_unicode() {
    let mut long_string = String::new();
    for _ in 0..BUFFER_SIZE / 4 - 5 {
        long_string.push('😁');
    }
    cutoff_string(&long_string);
    //Change alignment of string
    for _ in 0..3 {
        long_string.insert(0, 'a');
        cutoff_string(&long_string);
    }
}

#[test]
fn unicode_eof() {
    let test = [0xF0, 0x9F, 0x98];
    let token = Tokenizer::new(Cursor::new(test)).next();
    if let Some(Err(TokenizerError::Utf8Error)) = token {
    } else {
        panic!("{:?}", token)
    };
}
