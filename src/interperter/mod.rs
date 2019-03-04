/*
    Copyright 2018-2019 Alexander Eckhart

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

use crate::ast::AstListBuilder;
use crate::parser::{Parser, ParserError};
use crate::types::pair::ListFactory;
use crate::types::*;
use std::cell::RefCell;
use std::rc::Rc;

mod compiler;
pub use self::compiler::CompilerError;

mod environment;
use environment::{BaseEnvironment, MAIN_ENVIRONMENT};

mod builtin;
use builtin::BuiltinFunction;

#[derive(Copy, Clone, Debug)]
pub struct Statement {
    s_type: StatementType,
    arg: u32,
}

#[derive(Copy, Clone, Debug)]
enum StatementType {
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
    fn new(vars: Vec<Rc<RefCell<SchemeType>>>, function: Rc<SchemeFunction>) -> Self {
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
    lamadas: Vec<Rc<SchemeFunction>>,
}

impl SchemeFunction {
    fn new(argc: u32, is_vargs: bool) -> Self {
        Self {
            args: argc,
            is_vargs,
            captures: Vec::new(),
            code: Vec::new(),
            literals: Vec::new(),
            lamadas: Vec::new(),
        }
    }
}

fn run_vm(mut stack: Vec<StackFrame>) -> Result<SchemeType, RuntimeError> {
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
                    let child_function = function.lamadas[arg as usize].clone();

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

fn eval_with_environment(string: &str, env: &BaseEnvironment) -> Result<SchemeType, RuntimeError> {
    let parser = Parser::new(string);
    let mut object_builder = AstListBuilder::new();
    for object in parser {
        object_builder.push(object?)
    }
    let object = object_builder.build();

    if object.is_empty_list() {
        return eval("($gen_unspecified)");
    }

    let function = compiler::compile_function(&env.frame, object)?;
    let env_vars = env
        .bounded
        .iter()
        .map(|x| Rc::new(RefCell::new(x.clone())))
        .collect::<Vec<_>>();

    FunctionRef(FunctionRefInner::Derived(DerivedFunctionRef {
        function: Rc::new(function),
        captures: env_vars,
    }))
    .call(Vec::new())
}

pub fn eval(string: &str) -> Result<SchemeType, RuntimeError> {
    MAIN_ENVIRONMENT.with(|env| eval_with_environment(string, &env))
}

#[derive(Debug)]
pub enum RuntimeError {
    DivByZero,
    TypeError,
    EvalError(CompilerError),
    ReadError(ParserError),
    ArgError,
}

impl From<CompilerError> for RuntimeError {
    fn from(compile_err: CompilerError) -> RuntimeError {
        RuntimeError::EvalError(compile_err)
    }
}

impl From<ParserError> for RuntimeError {
    fn from(parse_err: ParserError) -> RuntimeError {
        RuntimeError::ReadError(parse_err)
    }
}

impl From<CastError> for RuntimeError {
    fn from(_: CastError) -> RuntimeError {
        RuntimeError::TypeError
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionRef(FunctionRefInner);

impl FunctionRef {
    pub fn call(self, args: Vec<SchemeType>) -> Result<SchemeType, RuntimeError> {
        let mut stack = Vec::new();

        let ret = self.0.call_with_stack(&mut stack, args)?;

        if let Some(value) = ret {
            Ok(value)
        } else {
            run_vm(stack)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum FunctionRefInner {
    Derived(DerivedFunctionRef),
    Builtin(BuiltinFunction),
}

impl FunctionRefInner {
    fn call_with_stack(
        self,
        stack: &mut Vec<StackFrame>,
        args: Vec<SchemeType>,
    ) -> Result<Option<SchemeType>, RuntimeError> {
        match self {
            FunctionRefInner::Builtin(func) => func.call_with_stack(stack, args),
            FunctionRefInner::Derived(func) => func.call_with_stack(stack, args),
        }
    }
}

#[derive(Clone, Debug)]
struct DerivedFunctionRef {
    function: Rc<SchemeFunction>,
    captures: Vec<Rc<RefCell<SchemeType>>>,
}

impl PartialEq for DerivedFunctionRef {
    fn eq(&self, other: &DerivedFunctionRef) -> bool {
        if !Rc::ptr_eq(&self.function, &other.function)
            || self.captures.len() != other.captures.len()
        {
            return false;
        }

        for (self_capture, other_capture) in self.captures.iter().zip(&other.captures) {
            if !Rc::ptr_eq(self_capture, other_capture) {
                return false;
            }
        }

        true
    }
}

impl DerivedFunctionRef {
    fn call_with_stack(
        self,
        stack: &mut Vec<StackFrame>,
        mut args: Vec<SchemeType>,
    ) -> Result<Option<SchemeType>, RuntimeError> {
        let argc = self.function.args as usize;

        if self.function.is_vargs {
            if args.len() < argc {
                return Err(RuntimeError::ArgError);
            }
        } else if args.len() != argc {
            return Err(RuntimeError::ArgError);
        }

        let mut env = Vec::new();
        for arg in args.drain(..argc) {
            env.push(Rc::new(RefCell::new(arg)))
        }

        if self.function.is_vargs {
            let mut extra_params = ListFactory::new();
            for extra_param in args {
                extra_params.push(extra_param)
            }
            env.push(Rc::new(RefCell::new(extra_params.build().into())))
        }

        for capture in self.captures {
            env.push(capture)
        }
        stack.push(StackFrame::new(env, self.function));
        Ok(None)
    }
}
