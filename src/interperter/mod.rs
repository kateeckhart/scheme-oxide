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
    argc: u32,
    ret: &mut SchemeType,
) -> Result<(), RuntimeError> {
    let function;
    if let SchemeType::Function(func) = object {
        function = func.0;
    } else {
        return Err(RuntimeError::TypeError);
    }

    function.call(stack, argc, ret)
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
    let mut ret_expr = SchemeType::EmptyList;
    'exec_loop: while let Some(s_frame) = stack.pop() {
        let mut frame = s_frame.top;
        let function = s_frame.function.clone();
        let code_len = function.code.len();
        let mut code_iter = function.code[s_frame.statement_num..code_len].iter();
        if s_frame.statement_num > 0 {
            frame.arg_stack.push(ret_expr.clone())
        }
        while let Some(statement) = code_iter.next() {
            let arg = statement.arg;
            match statement.s_type {
                StatementType::Get => frame.arg_stack.push(frame.vars[arg as usize].clone()),
                StatementType::Literal => frame
                    .arg_stack
                    .push(function.literals[arg as usize].clone()),
                StatementType::Call => {
                    let statement_num = code_len - code_iter.as_slice().len();
                    let new_function = frame.arg_stack.pop().unwrap();
                    stack.push(StackFrame {
                        top: frame,
                        statement_num,
                        function: function.clone(),
                    });
                    exec_function(new_function, &mut stack, arg, &mut ret_expr)?;
                    continue 'exec_loop;
                }
            }
        }
    }
    Ok(ret_expr)
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
}

impl From<CompilerError> for RuntimeError {
    fn from(compile_err: CompilerError) -> RuntimeError {
        RuntimeError::EvalError(compile_err)
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
        argc: u32,
        ret: &mut SchemeType,
    ) -> Result<(), RuntimeError> {
        match self {
            FunctionRefInner::Builtin(func) => func.call(stack, argc, ret),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum BuiltinFunction {
    Add,
}

impl BuiltinFunction {
    fn call(
        self,
        stack: &mut Vec<StackFrame>,
        argc: u32,
        ret: &mut SchemeType,
    ) -> Result<(), RuntimeError> {
        let arg_stack = &mut stack.last_mut().unwrap().top.arg_stack;
        match self {
            BuiltinFunction::Add => {
                let mut sum = 0;
                for _ in 0..argc {
                    let s_num = arg_stack.pop().unwrap();
                    if let SchemeType::Number(num) = s_num {
                        sum += num;
                    } else {
                        return Err(RuntimeError::TypeError);
                    }
                }
                *ret = SchemeType::Number(sum)
            }
        }
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
