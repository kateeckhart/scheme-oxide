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

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use types::*;

#[derive(Debug)]
pub enum RuntimeError {
    TypeError,
}

#[derive(Debug, Clone)]
pub struct FunctionRef(FunctionRefInner);

struct StackFrame {
    statement_num: usize,
    function: DerivedFunctionRef,
    arg_stack: Vec<SchemeType>
}

pub struct Stack(Vec<StackFrame>);

impl Stack {
    fn ret(&mut self, value: SchemeType) {
        let last = self.0.len() - 1;
        self.0[last].arg_stack.push(value)
    }
}

impl FunctionRef {
}

#[derive(Debug, Clone)]
enum FunctionRefInner {
    Derived(DerivedFunctionRef),
    Builtin(BuiltinFunction),
}

#[derive(Debug, Copy, Clone)]
enum BuiltinFunction {
    Add,
}

impl BuiltinFunction {
    fn call(self, stack: &mut Stack, mut args: Vec<SchemeType>) -> Result<(), RuntimeError> {
        match self {
            BuiltinFunction::Add => {
                let mut sum = 0;
                for s_num in args.drain(..) {
                    if let SchemeType::Number(num) = s_num {
                        sum += num;
                    } else {
                        return Err(RuntimeError::TypeError);
                    }
                    stack.ret(SchemeType::Number(sum))
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum CompilerError {
    IdentifierNotFound,
    SyntaxError,
    BadList,
}

impl From<pair::PairIterError> for CompilerError {
    fn from(_: pair::PairIterError) -> Self {
        CompilerError::BadList
    }
}

#[derive(Clone)]
enum SchemeMacro {}

#[derive(Clone)]
enum CompilerType {
    Runtime(u32),
    Macro(SchemeMacro),
}

#[derive(Clone)]
struct EnvironmentFrame {
    map: HashMap<String, CompilerType>,
    next_id: u32,
}

impl EnvironmentFrame {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            next_id: 0,
        }
    }

    fn len(&self) -> u32 {
        self.next_id
    }

    fn new_object(&mut self, name: &str) -> u32 {
        self.map
            .insert(name.into(), CompilerType::Runtime(self.next_id));
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn push_macro(&mut self, name: &str, s_macro: SchemeMacro) {
        self.map.insert(name.into(), CompilerType::Macro(s_macro));
    }

    fn lookup(&self, name: &str) -> Option<CompilerType> {
        self.map.get(name).cloned()
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
struct SchemeFunction {
    args: u32,
    is_vargs: bool,
    captures: Vec<u32>,
    code: Vec<Statement>,
    literals: Vec<SchemeType>,
}

struct PartialFunction {
    compiled_code: SchemeFunction,
    environment: EnvironmentFrame,
    parent: Option<Box<PartialFunction>>,
}

enum CompilerAction {
    Compile { expr_count: u32, code: SchemePair },
    CompileDone { expr_count: u32 },
    Call(u32),
    ProgramDone,
}

impl PartialFunction {
    fn traverse_macro(&mut self, name: &str) -> Result<Option<SchemeMacro>, CompilerError> {
        let mut function = Some(self);

        loop {
            if let Some(parent) = function.take().unwrap().parent.as_mut() {
                if let Some(compiler_type) = parent.environment.lookup(name) {
                    return Ok(match compiler_type {
                        CompilerType::Runtime(_) => None,
                        CompilerType::Macro(s_macro) => Some(s_macro),
                    });
                } else {
                    function = Some(parent);
                }
            } else {
                return Err(CompilerError::IdentifierNotFound);
            }
        }
    }

    fn lookup(&mut self, name: &str) -> Result<CompilerType, CompilerError> {
        if let Some(ident) = self.environment.lookup(name) {
            return Ok(ident);
        } else {
            let macro_or_none = self.traverse_macro(name)?;

            if let Some(s_macro) = macro_or_none {
                self.environment.push_macro(name, s_macro.clone());
                return Ok(CompilerType::Macro(s_macro));
            } else {
                let ret = self.environment.len();
                let mut function = Some(self);

                loop {
                    let func = function.take().unwrap();
                    func.environment.new_object(name);

                    if let Some(parent) = func.parent.as_mut() {
                        if let Some(compiler_type) = parent.environment.lookup(name) {
                            if let CompilerType::Runtime(ident) = compiler_type {
                                func.compiled_code.captures.push(ident);
                                return Ok(CompilerType::Runtime(ret));
                            } else {
                                unreachable!()
                            }
                        } else {
                            func.compiled_code.captures.push(parent.environment.len());
                            function = Some(parent);
                        }
                    } else {
                        unreachable!()
                    }
                }
            }
        }
    }
}

impl SchemeFunction {
    fn compile(
        base_environment: EnvironmentFrame,
        code: SchemePair,
    ) -> Result<Self, CompilerError> {
        let mut stack = vec![
            CompilerAction::ProgramDone,
            CompilerAction::Compile {
                expr_count: 0,
                code,
            },
        ];

        let mut function = PartialFunction {
            compiled_code: SchemeFunction {
                args: 0,
                is_vargs: false,
                captures: Vec::new(),
                code: Vec::new(),
                literals: Vec::new(),
            },
            environment: base_environment,
            parent: None,
        };

        'stack_loop: while let Some(action) = stack.pop() {
            match action {
                CompilerAction::Compile {
                    mut expr_count,
                    code,
                } => {
                    let mut expr_iter = code.iter();
                    while let Some(expr_or_err) = expr_iter.next() {
                        let expr = expr_or_err?;
                        expr_count += 1;
                        match expr {
                            SchemeType::Pair(pair) => {
                                let function_object = pair.get_car();
                                let function_name;
                                if let SchemeType::Symbol(func) = function_object {
                                    function_name = func;
                                } else {
                                    return Err(CompilerError::SyntaxError);
                                }

                                let calling_function = function.lookup(&function_name)?;
                                let args = if let SchemeType::Pair(a) = pair.get_cdr() {
                                    a
                                } else {
                                    return Err(CompilerError::BadList);
                                };

                                if let Some(rest_or_bad) = expr_iter.get_cdr() {
                                    if let SchemeType::Pair(rest) = rest_or_bad {
                                        stack.push(CompilerAction::Compile {
                                            expr_count,
                                            code: rest,
                                        })
                                    } else {
                                        return Err(CompilerError::BadList);
                                    }
                                } else {
                                    stack.push(CompilerAction::CompileDone { expr_count })
                                }

                                match calling_function {
                                    CompilerType::Runtime(ident) => {
                                        stack.push(CompilerAction::Call(ident));
                                        stack.push(CompilerAction::Compile {
                                            expr_count: 0,
                                            code: args,
                                        });
                                    }
                                    CompilerType::Macro(_) => unimplemented!(),
                                }

                                continue 'stack_loop;
                            }
                            _ => {
                                function.compiled_code.code.push(Statement {
                                    s_type: StatementType::Literal,
                                    arg: function.compiled_code.literals.len() as u32,
                                });
                                function.compiled_code.literals.push(expr);
                            }
                        }
                    }
                    stack.push(CompilerAction::CompileDone { expr_count })
                }
                CompilerAction::CompileDone { expr_count } => match stack.pop().unwrap() {
                    CompilerAction::Call(calling_function) => {
                        function.compiled_code.code.push(Statement {
                            s_type: StatementType::Literal,
                            arg: calling_function,
                        });
                        function.compiled_code.code.push(Statement {
                            s_type: StatementType::Call,
                            arg: expr_count,
                        });
                    }
                    CompilerAction::ProgramDone => return Ok(function.compiled_code),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
        unreachable!()
    }
}

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

#[derive(Clone, Debug)]
struct DerivedFunctionRef {
    function: Rc<SchemeFunction>,
    captures: Vec<Rc<RefCell<SchemeType>>>,
}

impl DerivedFunctionRef {
}

