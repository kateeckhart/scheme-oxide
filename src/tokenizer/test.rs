use super::{Token, Tokenizer, TokenizerError, BUFFER_SIZE};
use std::io::Cursor;

fn cutoff_string(long_string: &str) {
    let mut tokenizer = Tokenizer::new(Cursor::new(format!(
        r#""{}""{}""#,
        long_string, long_string
    )));
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
