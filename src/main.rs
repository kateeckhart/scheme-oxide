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

#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::io;
mod tokenizer;
use tokenizer::Tokenizer;

mod parser;
mod types;

fn main() {
    let token_stream = r#""testing" "\""((()))875467 alex i+ i9 "" + ..."#;
    for token in &mut Tokenizer::new(io::Cursor::new(token_stream)) {
        println!("{:?}", token.unwrap())
    }
}
