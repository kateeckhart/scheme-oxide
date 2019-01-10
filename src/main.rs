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

use std::io;

mod parser;
use crate::parser::datum::DatumParser;
mod types;
use crate::types::pair::ListFactory;

mod interperter;

//Transpose pollyfill
fn transpose_result<T, E>(result: Result<Option<T>, E>) -> Option<Result<T, E>> {
    match result {
        Ok(Some(x)) => Some(Ok(x)),
        Ok(None) => None,
        Err(e) => Some(Err(e)),
    }
}

fn transpose_option<T, E>(option: Option<Result<T, E>>) -> Result<Option<T>, E> {
    match option {
        Some(Ok(x)) => Ok(Some(x)),
        None => Ok(None),
        Some(Err(e)) => Err(e),
    }
}

fn main() {
    let token_stream = r#"(- (- 1 1 1) 1 4 7 (- 3))"#;
    let mut prog_factory = ListFactory::new();
    for object in DatumParser::new(io::Cursor::new(token_stream)) {
        prog_factory.push(object.unwrap())
    }
    let prog = prog_factory.build().unwrap();
    println!("{}", interperter::eval(prog).unwrap());
}
