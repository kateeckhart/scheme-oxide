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

#[derive(Clone, Debug, PartialEq)]
pub struct SchemePair(SchemeObject);

impl SchemePair {
    pub fn new(one: SchemeType, two: SchemeType) -> Self {
        SchemePair(SchemeObject::new(get_pair_type_id().into(), vec![one, two]))
    }

    pub fn one(object: SchemeType) -> Self {
        SchemePair::new(object, get_empty_list().into())
    }

    pub fn set_cdr(&self, cdr: SchemeType) {
        self.0.set_field(1, cdr).unwrap()
    }

    pub fn into_object(self) -> SchemeObject {
        self.0
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

    pub fn build(self) -> Option<SchemePair> {
        self.head
    }

    pub fn build_with_tail(self, object: SchemeType) -> Option<SchemePair> {
        if let Some(ref old_tail) = self.tail {
            old_tail.set_cdr(object)
        }
        self.build()
    }
}
