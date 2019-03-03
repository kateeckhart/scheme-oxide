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

use crate::types::*;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SchemePair(Rc<RefCell<(SchemeType, SchemeType)>>);

impl PartialEq for SchemePair {
    fn eq(&self, other: &SchemePair) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Display for SchemePair {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "(")?;
        let mut first = true;
        for item_or_err in self.iter() {
            if !first {
                write!(f, " ")?
            } else {
                first = false
            }

            if let Ok(item) = item_or_err {
                item.fmt(f)?
            } else if let Err(PairIterError::Improper(last)) = item_or_err {
                write!(f, ". {}", last)?;
            } else {
                return Err(fmt::Error);
            }
        }
        write!(f, ")")
    }
}

impl SchemePair {
    pub fn new(one: SchemeType, two: SchemeType) -> Self {
        SchemePair(Rc::new(RefCell::new((one, two))))
    }

    pub fn one(object: SchemeType) -> Self {
        SchemePair::new(object, SchemeType::EmptyList)
    }

    pub fn get_car(&self) -> SchemeType {
        self.0.borrow().0.clone()
    }

    pub fn get_cdr(&self) -> SchemeType {
        self.0.borrow().1.clone()
    }

    pub fn set_car(&self, car: SchemeType) {
        let mut self_ref = self.0.borrow_mut();
        self_ref.0 = car;
    }

    pub fn set_cdr(&self, cdr: SchemeType) {
        let mut self_ref = self.0.borrow_mut();
        self_ref.1 = cdr;
    }

    pub fn iter(&self) -> PairIter {
        PairIter::new(self.clone().into())
    }
}

#[derive(Clone)]
pub struct NullableSchemePair(Option<SchemePair>);

impl NullableSchemePair {
    pub fn into_option(self) -> Option<SchemePair> {
        self.0
    }
}

impl From<SchemePair> for NullableSchemePair {
    fn from(pair: SchemePair) -> Self {
        Self(Some(pair))
    }
}

impl From<Option<SchemePair>> for NullableSchemePair {
    fn from(pair: Option<SchemePair>) -> Self {
        Self(pair)
    }
}

pub struct PairIter {
    pair: SchemeType,
    tortoise: SchemeType,
    tortoise_step: bool,
}

impl PairIter {
    fn new(object: SchemeType) -> Self {
        Self {
            pair: object.clone(),
            tortoise: SchemeType::EmptyList,
            tortoise_step: false,
        }
    }
}

#[derive(Debug)]
pub enum PairIterError {
    Circular,
    Improper(SchemeType),
}

impl Iterator for PairIter {
    type Item = Result<SchemeType, PairIterError>;

    fn next(&mut self) -> Option<Self::Item> {
        let object = self.pair.clone();
        match object {
            SchemeType::Pair(pair) => {
                self.pair = pair.get_cdr();

                if let Ok(tort) = self.tortoise.clone().to_pair() {
                    if pair == tort {
                        return Some(Err(PairIterError::Circular));
                    }

                    if self.tortoise_step {
                        self.tortoise = tort.get_cdr()
                    }

                    self.tortoise_step = !self.tortoise_step;
                } else {
                    self.tortoise = pair.clone().into()
                }

                Some(Ok(pair.get_car()))
            }
            SchemeType::EmptyList => None,
            _ => {
                self.pair = SchemeType::EmptyList;
                Some(Err(PairIterError::Improper(object)))
            }
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct ListFactory {
    head: Option<SchemePair>,
    tail: Option<SchemePair>,
}

impl ListFactory {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, object: SchemeType) {
        let new_tail = SchemePair::one(object);
        if let Some(ref tail) = self.tail {
            tail.set_cdr(new_tail.clone().into());
        } else {
            self.head = Some(new_tail.clone());
        }
        self.tail = Some(new_tail);
    }

    pub fn build(self) -> NullableSchemePair {
        self.head.into()
    }

    pub fn build_with_tail(self, object: SchemeType) -> NullableSchemePair {
        if let Some(ref old_tail) = self.tail {
            old_tail.set_cdr(object)
        }
        self.build()
    }
}
