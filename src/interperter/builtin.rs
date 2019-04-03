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

use super::RuntimeError;
use crate::interperter::vm::StackFrame;
use crate::types::*;
use std::cmp::Ordering;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BuiltinFunction {
    Add,
    Mul,
    Cons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
    IsPair,
    Sub,
    Compare { invert: bool, mode: Ordering },
    Eqv,
    Quotient,
    Remainder,
    GenUnspecified,
}

impl BuiltinFunction {
    pub fn call_with_stack(
        self,
        stack: &mut Vec<StackFrame>,
        mut args: Vec<SchemeType>,
    ) -> Result<Option<SchemeType>, RuntimeError> {
        match self {
            BuiltinFunction::Add => {
                let mut sum = 0;
                for num in args {
                    sum += num.to_number()?
                }
                Ok(Some(SchemeType::Number(sum)))
            }
            BuiltinFunction::Mul => {
                let mut product = 0;
                for num in args {
                    product *= num.to_number()?
                }
                Ok(Some(SchemeType::Number(product)))
            }
            BuiltinFunction::Sub => {
                if args.len() == 1 {
                    Ok(Some(SchemeType::Number(-args[0].to_number()?)))
                } else if args.len() > 1 {
                    let mut iter = args.drain(..);
                    let mut difference = iter.next().unwrap().to_number()?;
                    for number in iter {
                        difference -= number.to_number()?
                    }
                    Ok(Some(SchemeType::Number(difference)))
                } else {
                    Err(RuntimeError::ArgError)
                }
            }
            BuiltinFunction::Compare { invert, mode } => {
                if args.len() < 2 {
                    return Err(RuntimeError::ArgError);
                }
                let mut iter = args.drain(..);
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
                Ok(Some(ret))
            }
            BuiltinFunction::Cons => {
                if args.len() != 2 {
                    return Err(RuntimeError::ArgError);
                }

                let cdr = args.pop().unwrap();
                let car = args.pop().unwrap();

                Ok(Some(
                    SchemePair::new(car, cdr).into(),
                ))
            }
            BuiltinFunction::Car => {
                if args.len() != 1 {
                    return Err(RuntimeError::ArgError);
                }

                Ok(Some(args[0].to_pair()?.get_car()))
            }
            BuiltinFunction::Cdr => {
                if args.len() != 1 {
                    return Err(RuntimeError::ArgError);
                }

                Ok(Some(args[0].to_pair()?.get_cdr()))
            }
            BuiltinFunction::SetCar => {
                if args.len() != 2 {
                    return Err(RuntimeError::ArgError);
                }

                let object = args.pop().unwrap();

                args[0].to_pair()?.set_car(object);

                BuiltinFunction::GenUnspecified.call_with_stack(stack, Vec::new())
            }
            BuiltinFunction::SetCdr => {
                if args.len() != 2 {
                    return Err(RuntimeError::ArgError);
                }

                let object = args.pop().unwrap();

                args[0].to_pair()?.set_cdr(object);

                BuiltinFunction::GenUnspecified.call_with_stack(stack, Vec::new())
            }
            BuiltinFunction::IsPair => {
                if args.len() != 1 {
                    return Err(RuntimeError::ArgError);
                }

                Ok(Some(SchemeType::Bool(args.pop().unwrap().is_pair())))
            }
            BuiltinFunction::Eqv => {
                if args.len() != 2 {
                    return Err(RuntimeError::ArgError);
                }

                Ok(Some(SchemeType::Bool(args[0] == args[1])))
            }
            BuiltinFunction::Quotient | BuiltinFunction::Remainder => {
                if args.len() != 2 {
                    return Err(RuntimeError::ArgError);
                }

                let b = args.pop().unwrap().to_number()?;
                let a = args.pop().unwrap().to_number()?;

                if b == 0 {
                    return Err(RuntimeError::DivByZero);
                }

                let res = match self {
                    BuiltinFunction::Quotient => a / b,
                    BuiltinFunction::Remainder => a % b,
                    _ => unreachable!(),
                };

                Ok(Some(SchemeType::Number(res)))
            }

            BuiltinFunction::GenUnspecified => Ok(Some(SchemeType::Bool(false))),
        }
    }
}
