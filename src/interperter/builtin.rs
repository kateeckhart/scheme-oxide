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

use super::{RuntimeError, StackFrame};
use crate::types::*;
use std::cmp::Ordering;

#[derive(Debug, Copy, Clone)]
pub enum BuiltinFunction {
    Add,
    Sub,
    Compare { invert: bool, mode: Ordering },
}

impl BuiltinFunction {
    pub fn call(
        self,
        stack: &mut Vec<StackFrame>,
        args: &[SchemeType],
        ret: &mut Option<SchemeType>,
    ) -> Result<(), RuntimeError> {
        *ret = Some(match self {
            BuiltinFunction::Add => {
                let mut sum = 0;
                for num in args {
                    sum += num.to_number()?
                }
                SchemeType::Number(sum)
            }
            BuiltinFunction::Sub => {
                if args.len() == 1 {
                    SchemeType::Number(-args[0].to_number()?)
                } else if args.len() > 1 {
                    let mut iter = args.iter();
                    let mut difference = iter.next().unwrap().to_number()?;
                    for number in iter {
                        difference -= number.to_number()?
                    }
                    SchemeType::Number(difference)
                } else {
                    return Err(RuntimeError::ArgError);
                }
            }
            BuiltinFunction::Compare { invert, mode } => {
                if args.len() < 2 {
                    return Err(RuntimeError::ArgError);
                }
                let mut iter = args.iter();
                let mut current = iter.next().unwrap().to_number()?;
                let mut ret = SchemeType::Bool(true);
                for raw_num in iter {
                    let num = raw_num.to_number()?;
                    let res = current.cmp(&num);
                    if (res == mode) == invert {
                        ret = SchemeType::Bool(false);
                        break;
                    }
                    current = num;
                }
                ret
            }
        });
        Ok(())
    }
}
