/*
    Copyright 2019 Alexander Eckhart

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

use std::cell::Cell;
use std::convert::Infallible;
use std::rc::Rc;
use std::str;

#[derive(Clone, Debug)]
pub struct SchemeString(Rc<SchemeStringInner>);

#[derive(Debug)]
struct SchemeStringInner {
    mutable: bool,
    chars: Box<[Cell<char>]>,
}

#[derive(Debug)]
pub enum StringSetError {
    IndexOutOfBounds,
    Immutable,
}

impl SchemeString {
    pub fn new(size: usize, fill: char) -> SchemeString {
        if size == 0 {
            return "".parse().unwrap();
        }

        let mut chars = Vec::with_capacity(size);

        for _ in 0..size {
            chars.push(Cell::new(fill))
        }

        SchemeString(Rc::new(SchemeStringInner {
            mutable: true,
            chars: chars.into_boxed_slice(),
        }))
    }

    pub fn len(&self) -> usize {
        self.0.chars.len()
    }

    pub fn get(&self, index: usize) -> Option<char> {
        self.0.chars.get(index).map(Cell::get)
    }

    pub fn set(&self, index: usize, c: char) -> Result<(), StringSetError> {
        if !self.0.mutable {
            return Err(StringSetError::Immutable);
        }

        self.0
            .chars
            .get(index)
            .map(|char_ptr| char_ptr.set(c))
            .ok_or(StringSetError::IndexOutOfBounds)
    }
}

impl PartialEq for SchemeString {
    fn eq(&self, other: &SchemeString) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl str::FromStr for SchemeString {
    //Will be the never type in future rust versions.
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Infallible> {
        thread_local! {
            static EMPTY_STRING: SchemeString = SchemeString(Rc::new(SchemeStringInner {
                mutable: false,
                chars: Vec::new().into_boxed_slice(),
            }))
        }

        if s == "" {
            return Ok(EMPTY_STRING.with(Clone::clone));
        }

        let mut chars = Vec::new();

        for c in s.chars() {
            chars.push(Cell::new(c))
        }
        Ok(SchemeString(Rc::new(SchemeStringInner {
            mutable: false,
            chars: chars.into_boxed_slice(),
        })))
    }
}
