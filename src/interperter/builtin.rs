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
    Sub,
    Compare { invert: bool, mode: Ordering },
    Eqv,
    Quotient,
    Remainder,
    GenUnspecified,
    Error,
    IsObject,
    IsNumber,
    IsChar,
    IsString,
    GetTypeId,
    GetField,
    SetField,
    GetChar,
    SetChar,
    NewObject,
    NewString,
    StringLen,
    WriteChar,
}

fn gen_unspecified() -> SchemeType {
    get_false().into()
}

fn assert_args<T>(args: &[T], argc: usize, is_vargs: bool) -> Result<(), RuntimeError> {
    if (is_vargs && args.len() < argc) || (!is_vargs && args.len() != argc) {
        Err(RuntimeError::ArgError)
    } else {
        Ok(())
    }
}

impl BuiltinFunction {
    pub fn call_with_stack(
        self,
        _stack: &mut Vec<StackFrame>,
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
                let mut product = 1;
                for num in args {
                    product *= num.to_number()?
                }
                Ok(Some(SchemeType::Number(product)))
            }
            BuiltinFunction::Sub => {
                if args.len() == 1 {
                    Ok(Some(SchemeType::Number(-args[0].to_number()?)))
                } else if args.len() > 1 {
                    let mut iter = args.into_iter();
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
                assert_args(&args, 2, true)?;

                let mut iter = args.into_iter();
                let mut current = iter.next().unwrap().to_number()?;
                let mut ret = get_true();
                for raw_num in iter {
                    let num = raw_num.to_number()?;
                    let res = current.cmp(&num);
                    if (res == mode) == invert {
                        ret = get_false();
                        break;
                    }
                    current = num;
                }
                Ok(Some(ret.into()))
            }
            BuiltinFunction::Eqv => {
                assert_args(&args, 2, false)?;

                Ok(Some((args[0] == args[1]).into()))
            }
            BuiltinFunction::Quotient | BuiltinFunction::Remainder => {
                assert_args(&args, 2, false)?;

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

            BuiltinFunction::GenUnspecified => Ok(Some(gen_unspecified())),
            BuiltinFunction::Error => Err(RuntimeError::AssertFailed),
            BuiltinFunction::IsObject => {
                assert_args(&args, 1, false)?;

                let object = args.pop().unwrap();
                Ok(Some(
                    if let SchemeType::Object(_) = object {
                        true
                    } else {
                        false
                    }
                    .into(),
                ))
            }
            BuiltinFunction::IsNumber => {
                assert_args(&args, 1, false)?;

                let object = args.pop().unwrap();
                Ok(Some(
                    if let SchemeType::Number(_) = object {
                        true
                    } else {
                        false
                    }
                    .into(),
                ))
            }
            BuiltinFunction::IsChar => {
                assert_args(&args, 1, false)?;

                let object = args.pop().unwrap();
                Ok(Some(
                    if let SchemeType::Char(_) = object {
                        true
                    } else {
                        false
                    }
                    .into(),
                ))
            }
            BuiltinFunction::IsString => {
                assert_args(&args, 1, false)?;

                let object = args.pop().unwrap();
                Ok(Some(
                    if let SchemeType::String(_) = object {
                        true
                    } else {
                        false
                    }
                    .into(),
                ))
            }
            BuiltinFunction::GetTypeId => {
                assert_args(&args, 1, false)?;

                let object = args.pop().unwrap().into_object()?;
                Ok(Some(object.get_type_id()))
            }
            BuiltinFunction::GetField => {
                assert_args(&args, 2, false)?;

                let index = args.pop().unwrap().to_index()?;
                let object = args.pop().unwrap().into_object()?;

                object
                    .get_field(index)
                    .ok_or(RuntimeError::OutOfBounds)
                    .map(Some)
            }
            BuiltinFunction::SetField => {
                assert_args(&args, 3, false)?;

                let field_value = args.pop().unwrap();
                let index = args.pop().unwrap().to_index()?;
                let object = args.pop().unwrap().into_object()?;

                object
                    .set_field(index, field_value)
                    .map(|_| Some(gen_unspecified()))
                    .map_err(|_| RuntimeError::OutOfBounds)
            }
            BuiltinFunction::GetChar => {
                assert_args(&args, 2, false)?;

                let index = args.pop().unwrap().to_index()?;
                let string = args.pop().unwrap().into_string()?;

                string
                    .get(index)
                    .ok_or(RuntimeError::OutOfBounds)
                    .map(|c| Some(SchemeType::Char(c)))
            }
            BuiltinFunction::SetChar => {
                assert_args(&args, 3, false)?;

                let c = args.pop().unwrap().to_char()?;
                let index = args.pop().unwrap().to_index()?;
                let string = args.pop().unwrap().into_string()?;

                string
                    .set(index, c)
                    .map(|_| Some(gen_unspecified()))
                    .map_err(|_| RuntimeError::AssertFailed)
            }
            BuiltinFunction::NewObject => {
                assert_args(&args, 1, true)?;

                let type_id = args.remove(0);
                Ok(Some(SchemeObject::new(type_id, args).into()))
            }
            BuiltinFunction::NewString => {
                let fill;

                if args.len() == 1 {
                    fill = '\0';
                } else if args.len() == 2 {
                    fill = args.pop().unwrap().to_char()?;
                } else {
                    return Err(RuntimeError::ArgError);
                }

                let size = args.pop().unwrap().to_index()?;

                Ok(Some(SchemeString::new(size, fill).into()))
            }
            BuiltinFunction::StringLen => {
                assert_args(&args, 1, false)?;

                let string = args.pop().unwrap().into_string()?;

                Ok(Some(string.len().into()))
            }
            BuiltinFunction::WriteChar => {
                assert_args(&args, 1, false)?;

                let c = args.pop().unwrap().to_char()?;

                print!("{}", c);
                Ok(Some(gen_unspecified()))
            }
        }
    }
}
