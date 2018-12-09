use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::{Display, Formatter, self};

mod pair;
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
