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

use crate::interperter::eval;
use crate::types::pair::PairIterError;
use crate::types::*;

#[test]
fn add_zero() {
    let res = eval("(+)").unwrap().to_number().unwrap();
    assert_eq!(0, res);
}

#[test]
fn add_one() {
    for x in 0..256 {
        let res = eval(&format!("(+ {})", x)).unwrap().to_number().unwrap();
        assert_eq!(x, res);
    }
}

#[test]
fn add_two() {
    for x in 0..256 {
        for y in 0..256 {
            let res = eval(&format!("(+ {} {})", x, y))
                .unwrap()
                .to_number()
                .unwrap();
            assert_eq!(x + y, res);
        }
    }
}

#[test]
fn add_three() {
    for x in 0..50 {
        for y in 0..50 {
            for z in 0..50 {
                let res = eval(&format!("(+ {} {} {})", x, y, z))
                    .unwrap()
                    .to_number()
                    .unwrap();
                assert_eq!(x + y + z, res);
            }
        }
    }
}

#[test]
fn inf_list() {
    let list = SchemePair::one(SchemeType::EmptyList);
    list.set_cdr(list.clone().into());

    let mut list_iter = list.iter();

    for _ in 0..5000 {
        match list_iter.next() {
            Some(Err(PairIterError::Circular)) => return,
            _ => (),
        }
    }
    panic!()
}

#[test]
fn list_fun() {
    assert_eq!(eval("(list)").unwrap(), SchemeType::EmptyList);
    let list: Vec<_> = eval("(list 1 2 3 4 5)")
        .unwrap()
        .to_pair()
        .unwrap()
        .iter()
        .map(|x| x.unwrap().to_number().unwrap())
        .collect();
    let list_ref: &[i64] = &list;
    let expected_list: &[i64] = &[1, 2, 3, 4, 5];
    assert_eq!(list_ref, expected_list);
}
