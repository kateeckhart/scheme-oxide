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

use super::{DerivedFunctionRef, FunctionRef, FunctionRefInner, RuntimeError};
use crate::types::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Copy, Clone, Debug)]
pub struct Statement {
    pub s_type: StatementType,
    pub arg: u32,
}

#[derive(Copy, Clone, Debug)]
pub enum StatementType {
    Get,
    Set,
    Literal,
    Call,
    Tail,
    Discard,
    Lamada,
    Branch,
    BranchIfFalse,
}

struct StackTop {
    arg_stack: Vec<SchemeType>,
    vars: Vec<Rc<RefCell<SchemeType>>>,
}

pub struct StackFrame {
    top: StackTop,
    statement_num: usize,
    function: Rc<SchemeFunction>,
}

impl StackFrame {
    pub fn new(vars: Vec<Rc<RefCell<SchemeType>>>, function: Rc<SchemeFunction>) -> Self {
        Self {
            top: StackTop {
                arg_stack: Vec::new(),
                vars,
            },
            statement_num: 0,
            function,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SchemeFunction {
    args: u32,
    is_vargs: bool,
    captures: Vec<u32>,
    code: Vec<Statement>,
    literals: Vec<SchemeType>,
    lambdas: Vec<Rc<SchemeFunction>>,
}

impl SchemeFunction {
    pub fn new(argc: u32, is_vargs: bool) -> Self {
        Self {
            args: argc,
            is_vargs,
            captures: Vec::new(),
            code: Vec::new(),
            literals: Vec::new(),
            lambdas: Vec::new(),
        }
    }

    pub fn append_code(&mut self, mut code: Vec<Statement>) {
        self.code.append(&mut code)
    }

    pub fn new_capture(&mut self, capture: u32) {
        self.captures.push(capture)
    }

    pub fn new_literal(&mut self, literal: SchemeType) {
        self.literals.push(literal)
    }

    pub fn new_lambda(&mut self, lambda: SchemeFunction) {
        self.lambdas.push(Rc::new(lambda))
    }

    pub fn literal_len(&self) -> usize {
        self.literals.len()
    }

    pub fn lambda_len(&self) -> usize {
        self.lambdas.len()
    }

    pub fn get_args(&self) -> u32 {
        self.args
    }

    pub fn is_vargs(&self) -> bool {
        self.is_vargs
    }
}

pub fn run_vm(mut stack: Vec<StackFrame>) -> Result<SchemeType, RuntimeError> {
    let mut ret_expr = None;
    'exec_loop: while let Some(s_frame) = stack.pop() {
        let mut frame = s_frame.top;
        let function = s_frame.function;
        let mut code_iter = function.code[s_frame.statement_num..].iter();
        if ret_expr.is_some() {
            frame.arg_stack.push(ret_expr.take().unwrap())
        }
        while let Some(statement) = code_iter.next() {
            let arg = statement.arg;
            match statement.s_type {
                StatementType::Get => frame
                    .arg_stack
                    .push(frame.vars[arg as usize].borrow().clone()),
                StatementType::Set => {
                    frame.vars[arg as usize].replace(frame.arg_stack.pop().unwrap());
                }
                StatementType::Literal => frame
                    .arg_stack
                    .push(function.literals[arg as usize].clone()),
                StatementType::Call | StatementType::Tail => {
                    let statement_num = function.code.len() - code_iter.as_slice().len();
                    let mut drain = frame
                        .arg_stack
                        .drain(frame.arg_stack.len() - (arg as usize) - 1..);
                    let new_function = drain.next().unwrap();
                    let args = drain.collect::<Vec<_>>();

                    if let StatementType::Call = statement.s_type {
                        stack.push(StackFrame {
                            top: frame,
                            statement_num,
                            function: function.clone(),
                        });
                    }

                    ret_expr = new_function
                        .to_function()?
                        .0
                        .call_with_stack(&mut stack, args)?;
                    continue 'exec_loop;
                }
                StatementType::Discard => {
                    frame.arg_stack.pop();
                }
                StatementType::Lamada => {
                    let child_function = function.lambdas[arg as usize].clone();

                    let mut captures = Vec::new();

                    for capture in child_function.captures.iter() {
                        captures.push(frame.vars[*capture as usize].clone())
                    }

                    frame.arg_stack.push(SchemeType::Function(FunctionRef(
                        FunctionRefInner::Derived(DerivedFunctionRef {
                            function: child_function,
                            captures,
                        }),
                    )))
                }
                StatementType::Branch | StatementType::BranchIfFalse => {
                    let branch = if let StatementType::Branch = statement.s_type {
                        true
                    } else {
                        !frame.arg_stack.pop().unwrap().to_bool()
                    };

                    if branch {
                        let statement_num = function.code.len() - code_iter.as_slice().len();
                        code_iter = function.code[statement_num + arg as usize..].iter();
                    }
                }
            }
        }
        ret_expr = Some(frame.arg_stack.pop().unwrap())
    }
    Ok(ret_expr.unwrap())
}
