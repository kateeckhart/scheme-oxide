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
fn list_fun() {
    assert_eq!(eval("(list)").unwrap(), get_empty_list());
    assert_eq!(
        eval("(equal? (list 1 2 (list 3 4) 5 6) '(1 2 (3 4) 5 6))").unwrap(),
        get_true()
    );
}
