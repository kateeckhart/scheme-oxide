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

use std::cell::RefCell;
use std::rc::Rc;

use builtin::BuiltinFunction;
use runtime_environment::{BaseEnvironment, SCHEME_ENVIRONMENT};
use vm::{run_vm, SchemeFunction, StackFrame};

use crate::ast::AstNode;
use crate::parser::ParserError;
use crate::types::*;

pub use self::compiler::CompilerError;

mod builtin;
mod compiler;
pub mod runtime_environment;
mod vm;

fn eval_with_environment(
    nodes: AstNode,
    env: &BaseEnvironment,
) -> Result<SchemeType, RuntimeError> {
    let function = compiler::compile_function(&env.frame, nodes)?;
    let env_vars = env.bounded.clone();

    FunctionRef(FunctionRefInner::Derived(DerivedFunctionRef {
        function: Rc::new(function),
        captures: env_vars,
    }))
    .call(Vec::new())
}

pub fn eval(string: &str) -> Result<SchemeType, RuntimeError> {
    SCHEME_ENVIRONMENT.with(|env| env.eval_str(string))
}

#[derive(Debug)]
pub enum RuntimeError {
    AssertFailed,
    OutOfBounds,
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
        let argc = self.function.get_args() as usize;

        if self.function.is_vargs() {
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

        if self.function.is_vargs() {
            let mut extra_params = ListFactory::new(true);
            for extra_param in args {
                extra_params.push(extra_param)
            }
            env.push(Rc::new(RefCell::new(extra_params.build())))
        }

        for capture in self.captures {
            env.push(capture)
        }
        stack.push(StackFrame::new(env, self.function));
        Ok(None)
    }
}
