/*
    Copyright 2018 Alexander Eckhart

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

use crate::types::*;
use std::cell::RefCell;
use std::rc::Rc;

mod compiler;
pub use self::compiler::CompilerError;
use self::compiler::EnvironmentFrame;

#[derive(Copy, Clone, Debug)]
struct Statement {
    s_type: StatementType,
    arg: u32,
}

#[derive(Copy, Clone, Debug)]
enum StatementType {
    Get,
    Literal,
    Call,
}

struct StackTop {
    arg_stack: Vec<SchemeType>,
    vars: Vec<SchemeType>,
}

impl StackTop {
    fn new(vars: Vec<SchemeType>) -> Self {
        Self {
            arg_stack: Vec::new(),
            vars,
        }
    }
}

struct StackFrame {
    top: StackTop,
    statement_num: usize,
    function: Rc<SchemeFunction>,
}

#[derive(Clone, Debug)]
pub struct SchemeFunction {
    args: u32,
    is_vargs: bool,
    captures: Vec<u32>,
    code: Vec<Statement>,
    literals: Vec<SchemeType>,
}

fn exec_function(
    object: SchemeType,
    stack: &mut Vec<StackFrame>,
    args: &[SchemeType],
    ret: &mut Option<SchemeType>,
) -> Result<(), RuntimeError> {
    let function;
    if let SchemeType::Function(func) = object {
        function = func.0;
    } else {
        return Err(RuntimeError::TypeError);
    }

    function.call(stack, args, ret)
}

fn exec_top_function(
    top: Rc<SchemeFunction>,
    env: Vec<SchemeType>,
) -> Result<SchemeType, RuntimeError> {
    if top.args != 0 || top.is_vargs {
        panic!("Not a top level function.");
    }
    let mut stack = vec![StackFrame {
        top: StackTop::new(env),
        statement_num: 0,
        function: top,
    }];
    let mut ret_expr = None;
    'exec_loop: while let Some(s_frame) = stack.pop() {
        let mut frame = s_frame.top;
        let function = s_frame.function.clone();
        let mut code_iter = function.code[s_frame.statement_num..].iter();
        if s_frame.statement_num > 0 {
            frame.arg_stack.push(ret_expr.take().unwrap())
        }
        while let Some(statement) = code_iter.next() {
            let arg = statement.arg;
            match statement.s_type {
                StatementType::Get => frame.arg_stack.push(frame.vars[arg as usize].clone()),
                StatementType::Literal => frame
                    .arg_stack
                    .push(function.literals[arg as usize].clone()),
                StatementType::Call => {
                    let statement_num = function.code.len() - code_iter.as_slice().len();
                    let new_function = frame.arg_stack.pop().unwrap();
                    let args = frame
                        .arg_stack
                        .drain(frame.arg_stack.len() - (arg as usize)..)
                        .collect::<Vec<_>>();
                    stack.push(StackFrame {
                        top: frame,
                        statement_num,
                        function: function.clone(),
                    });
                    exec_function(new_function, &mut stack, &args, &mut ret_expr)?;
                    continue 'exec_loop;
                }
            }
        }
        ret_expr = Some(frame.arg_stack.pop().unwrap())
    }
    Ok(ret_expr.unwrap())
}

pub fn eval(object: SchemePair) -> Result<SchemeType, RuntimeError> {
    let function = MAIN_ENVIRONMENT.with(|env| compiler::compile_function(&env.frame, object));
    let env = MAIN_ENVIRONMENT.with(|env| env.bounded.clone());
    exec_top_function(Rc::new(function?), env)
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeError,
    EvalError(CompilerError),
    ArgError,
}

impl From<CompilerError> for RuntimeError {
    fn from(compile_err: CompilerError) -> RuntimeError {
        RuntimeError::EvalError(compile_err)
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
        args: &[SchemeType],
        ret: &mut Option<SchemeType>,
    ) -> Result<(), RuntimeError> {
        match self {
            FunctionRefInner::Builtin(func) => func.call(stack, args, ret),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum BuiltinFunction {
    Add,
    Sub,
}

impl BuiltinFunction {
    fn call(
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
        });
        Ok(())
    }
}

struct BaseEnvironment {
    frame: EnvironmentFrame,
    bounded: Vec<SchemeType>,
}

impl BaseEnvironment {
    fn new() -> Self {
        Self {
            frame: EnvironmentFrame::new(),
            bounded: Vec::new(),
        }
    }

    fn push_object(&mut self, name: &str, object: SchemeType) {
        self.frame.new_object(name);
        self.bounded.push(object)
    }

    fn push_builtin_function(&mut self, name: &str, function: BuiltinFunction) {
        self.push_object(
            name,
            SchemeType::Function(FunctionRef(FunctionRefInner::Builtin(function))),
        )
    }
}

fn gen_scheme_environment() -> BaseEnvironment {
    let mut ret = BaseEnvironment::new();

    ret.push_builtin_function("+", BuiltinFunction::Add);
    ret.push_builtin_function("-", BuiltinFunction::Sub);
    ret
}

fn gen_main_environment() -> BaseEnvironment {
    gen_scheme_environment()
}

thread_local! {
    static SCHEME_ENVIORNMENT: BaseEnvironment = gen_scheme_environment();

    static MAIN_ENVIRONMENT: BaseEnvironment = gen_main_environment();
}

#[derive(Clone, Debug)]
struct DerivedFunctionRef {
    function: Rc<SchemeFunction>,
    captures: Vec<Rc<RefCell<SchemeType>>>,
}
