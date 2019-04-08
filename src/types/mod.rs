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

pub mod pair;
pub use self::pair::SchemePair;
use crate::interperter::FunctionRef;
mod object;
pub use self::object::SchemeObject;

macro_rules! gen_singleton {
    (pub $name:ident) => {
        pub fn $name() -> SchemeType {
            thread_local! {
                static SINGLETON: SchemeObject = SchemeObject::unique_new()
            }
            SINGLETON.with(|s| s.clone().into())
        }
    };
}

gen_singleton!(pub get_empty_list);
gen_singleton!(pub get_true);
gen_singleton!(pub get_false);

#[derive(Clone, PartialEq, Debug)]
pub enum SchemeType {
    Function(FunctionRef),
    Number(i64),
    Pair(SchemePair),
    String(String),
    Symbol(String),
    Object(SchemeObject),
}

#[derive(Clone, Debug)]
pub struct CastError;

impl SchemeType {
    pub fn is_pair(&self) -> bool {
        if let SchemeType::Pair(_) = self {
            true
        } else {
            false
        }
    }

    pub fn to_number(&self) -> Result<i64, CastError> {
        if let SchemeType::Number(num) = self {
            Ok(*num)
        } else {
            Err(CastError)
        }
    }

    pub fn to_bool(&self) -> bool {
        !(*self == get_false())
    }

    pub fn to_pair(&self) -> Result<SchemePair, CastError> {
        Ok(match self {
            SchemeType::Pair(ret) => ret.clone(),
            _ => return Err(CastError),
        })
    }

    pub fn to_function(&self) -> Result<FunctionRef, CastError> {
        Ok(match self {
            SchemeType::Function(func) => func.clone(),
            _ => return Err(CastError),
        })
    }
}

impl From<SchemePair> for SchemeType {
    fn from(pair: SchemePair) -> SchemeType {
        SchemeType::Pair(pair)
    }
}

impl From<Option<SchemePair>> for SchemeType {
    fn from(pair: Option<SchemePair>) -> SchemeType {
        match pair {
            Some(x) => x.into(),
            None => get_empty_list(),
        }
    }
}

impl From<FunctionRef> for SchemeType {
    fn from(func: FunctionRef) -> Self {
        SchemeType::Function(func)
    }
}

impl From<SchemeObject> for SchemeType {
    fn from(object: SchemeObject) -> Self {
        SchemeType::Object(object)
    }
}

impl From<bool> for SchemeType {
    fn from(is_true: bool) -> Self {
        if is_true {
            get_true()
        } else {
            get_false()
        }
    }
}
