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

use crate::parser::{Parser, ParserError};
use crate::types::pair::ListFactory;
use crate::types::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::thread::LocalKey;

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

fn exec_function(
    object: SchemeType,
    stack: &mut Vec<StackFrame>,
    args: Vec<SchemeType>,
) -> Result<Option<SchemeType>, RuntimeError> {
    let function;
    if let SchemeType::Function(func) = object {
        function = func.0;
    } else {
        return Err(RuntimeError::TypeError);
    }

    function.call(stack, args)
}

fn exec_top_function(
    top: Rc<SchemeFunction>,
    env: Vec<Rc<RefCell<SchemeType>>>,
) -> Result<SchemeType, RuntimeError> {
    if top.args != 0 || top.is_vargs {
        panic!("Not a top level function.");
    }
    let mut stack = vec![StackFrame::new(env, top)];
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

                    ret_expr = exec_function(new_function, &mut stack, args)?;
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

fn generate_unspecified() -> SchemeType {
    SchemeType::Bool(false)
}

fn eval_with_environment(
    string: &str,
    raw_env: &'static LocalKey<BaseEnvironment>,
) -> Result<SchemeType, RuntimeError> {
    let parser = Parser::new(string);
    let mut object_builder = ListFactory::new();
    for object in parser {
        object_builder.push(object?)
    }
    let object_or_none = object_builder.build().into_option();

    if object_or_none.is_none() {
        return Ok(generate_unspecified());
    }

    let object = object_or_none.unwrap();

    let function = raw_env.with(|env| compiler::compile_function(&env.frame, object));
    let env = raw_env.with(|env| {
        env.bounded
            .iter()
            .map(|x| Rc::new(RefCell::new(x.clone())))
            .collect::<Vec<_>>()
    });
    exec_top_function(Rc::new(function?), env)
}

pub fn eval(string: &str) -> Result<SchemeType, RuntimeError> {
    eval_with_environment(string, &MAIN_ENVIRONMENT)
}

#[derive(Debug)]
pub enum RuntimeError {
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

#[derive(Debug, Clone)]
pub struct FunctionRef(FunctionRefInner);

#[derive(Debug, Clone)]
enum FunctionRefInner {
    Derived(DerivedFunctionRef),
    Builtin(BuiltinFunction),
}

impl FunctionRefInner {
    fn call(
        self,
        stack: &mut Vec<StackFrame>,
        args: Vec<SchemeType>,
    ) -> Result<Option<SchemeType>, RuntimeError> {
        match self {
            FunctionRefInner::Builtin(func) => func.call(stack, args),
            FunctionRefInner::Derived(func) => func.call(stack, args),
        }
    }
}

#[derive(Clone, Debug)]
struct DerivedFunctionRef {
    function: Rc<SchemeFunction>,
    captures: Vec<Rc<RefCell<SchemeType>>>,
}

impl DerivedFunctionRef {
    fn call(
        self,
        stack: &mut Vec<StackFrame>,
        args: Vec<SchemeType>,
    ) -> Result<Option<SchemeType>, RuntimeError> {
        if self.function.args as usize != args.len() {
            return Err(RuntimeError::ArgError);
        }

        let mut env = Vec::new();
        for arg in args {
            env.push(Rc::new(RefCell::new(arg)))
        }
        for capture in self.captures {
            env.push(capture)
        }
        stack.push(StackFrame::new(env, self.function));
        Ok(None)
    }
}
