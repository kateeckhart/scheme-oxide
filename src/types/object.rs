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
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SchemeObject(Rc<RefCell<SchemeObjectInner>>);

#[derive(Debug)]
struct SchemeObjectInner {
    type_id: SchemeType,
    fields: Box<[SchemeType]>,
}

impl SchemeObject {
    fn new(type_id: SchemeType, size: usize, fill: SchemeType) -> SchemeObject {
        let mut fields = Vec::with_capacity(size);

        if size != 0 {
            for _ in 0..size - 1 {
                fields.push(fill.clone())
            }

            fields.push(fill)
        }

        SchemeObject(Rc::new(RefCell::new(SchemeObjectInner {
            type_id,
            fields: fields.into_boxed_slice(),
        })))
    }

    //Create an object with a unique address in memory.
    //For the purpose of creating type ids.
    pub fn unique_new() -> SchemeObject {
        SchemeObject::new(SchemeType::Bool(false), 0, SchemeType::Bool(false))
    }

    fn get_type_id(&self) -> SchemeType {
        self.0.borrow().type_id.clone()
    }

    fn get_field(&self, index: usize) -> Option<SchemeType> {
        self.0.borrow().fields.get(index).cloned()
    }
}

impl PartialEq for SchemeObject {
    fn eq(&self, other: &SchemeObject) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
