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

use std::fmt::{self, Display, Formatter};

pub mod pair;
pub use self::pair::SchemePair;

#[derive(Clone, Debug)]
pub enum SchemeType {
    Pair(SchemePair),
    Number(i64),
    String(String),
    Symbol(String),
    EmptyList,
}

impl Display for SchemeType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            SchemeType::Pair(pair) => pair.fmt(f)?,
            SchemeType::Number(num) => num.fmt(f)?,
            SchemeType::String(string) => {
                write!(f, "\"")?;
                for c in string.chars() {
                    match c {
                        '"' => write!(f, r#"\""#)?,
                        '\\' => write!(f, r"\\")?,
                        _ => c.fmt(f)?,
                    }
                }
                write!(f, "\"")?;
            }
            SchemeType::Symbol(symbol) => symbol.fmt(f)?,
            SchemeType::EmptyList => write!(f, "()")?,
        }
        Ok(())
    }
}
