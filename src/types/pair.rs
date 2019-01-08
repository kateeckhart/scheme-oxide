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

use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use crate::types::*;

#[derive(Clone, Debug)]
pub struct SchemePair(Rc<RefCell<(SchemeType, SchemeType)>>);

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
            } else if let Err(PairIterError::Improper((next_to_last, last))) = item_or_err {
                write!(f, "{} . {}", next_to_last, last)?;
                break;
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
        PairIter {
            pair: Some(self.clone()),
        }
    }

    pub fn len(&self) -> Result<usize, PairIterError> {
        let mut count = 0;
        for object in self.iter() {
            object?;
            count += 1;
        }
        Ok(count)
    }
}

pub struct PairIter {
    pair: Option<SchemePair>,
}

impl PairIter {
    pub fn get_rest(&self) -> Option<SchemePair> {
        self.pair.clone()
    }
}

#[derive(Debug)]
pub enum PairIterError {
    Circular,
    Improper((SchemeType, SchemeType)),
}

impl Iterator for PairIter {
    type Item = Result<SchemeType, PairIterError>;

    fn next(&mut self) -> Option<Self::Item> {
        let pair = if let Some(ref pair) = self.pair {
            pair.0.clone()
        } else {
            return None;
        };
        let pair_ref = RefCell::borrow(&pair);

        Some(if let SchemeType::Pair(ref next) = pair_ref.1 {
            self.pair = Some(next.clone());
            Ok(pair_ref.0.clone())
        } else if let SchemeType::EmptyList = pair_ref.1 {
            self.pair = None;
            Ok(pair_ref.0.clone())
        } else {
            Err(PairIterError::Improper((
                pair_ref.0.clone(),
                pair_ref.1.clone(),
            )))
        })
    }
}

pub struct ListFactory {
    head: Option<SchemePair>,
    tail: Option<SchemePair>,
}

impl ListFactory {
    pub fn new() -> Self {
        Self {
            head: None,
            tail: None,
        }
    }
    pub fn push(&mut self, object: SchemeType) {
        let new_tail = SchemePair::new(object, SchemeType::EmptyList);
        if let Some(ref tail) = self.tail {
            tail.set_cdr(new_tail.clone().into());
        } else {
            self.head = Some(new_tail.clone());
        }
        self.tail = Some(new_tail);
    }

    pub fn build(self) -> Option<SchemePair> {
        self.head
    }
}
