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

use super::{SchemeFunction, Statement, StatementType};
use crate::types::*;
use std::collections::HashMap;
use std::mem::replace;

#[derive(Clone)]
pub struct EnvironmentFrame {
    map: HashMap<String, CompilerType>,
    next_id: u32,
}

impl EnvironmentFrame {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            next_id: 0,
        }
    }

    fn len(&self) -> u32 {
        self.next_id
    }

    pub fn new_object(&mut self, name: &str) -> u32 {
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
pub enum SchemeMacro {}

#[derive(Clone)]
enum CompilerType {
    Runtime(u32),
    Macro(SchemeMacro),
}

struct PartialFunction {
    compiled_code: SchemeFunction,
    environment: EnvironmentFrame,
    parent: Option<Box<PartialFunction>>,
}

#[derive(Debug)]
enum CompilerAction {
    Compile { code: SchemePair },
    FunctionDone,
    EmitAsm { statements: Vec<Statement> },
    ExprDone,
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

pub fn compile_function(
    base_environment: &EnvironmentFrame,
    code: SchemePair,
) -> Result<SchemeFunction, CompilerError> {
    let mut stack = vec![
        CompilerAction::FunctionDone,
        CompilerAction::Compile { code },
    ];

    let mut function = PartialFunction {
        compiled_code: SchemeFunction::default(),
        environment: base_environment.clone(),
        parent: None,
    };

    let mut current_code_block = Vec::new();

    'stack_loop: while let Some(action) = stack.pop() {
        match action {
            CompilerAction::Compile { code } => {
                let mut expr_iter = code.iter();
                while let Some(expr_or_err) = expr_iter.next() {
                    let expr = expr_or_err?;
                    match expr {
                        SchemeType::Pair(pair) => {
                            let function_object = pair.get_car();
                            if let SchemeType::Symbol(function_name) = function_object {
                                let calling_function = function.lookup(&function_name)?;
                                if let CompilerType::Macro(_) = calling_function {
                                    unimplemented!()
                                }
                            }

                            if let Some(rest) = expr_iter.get_rest()? {
                                stack.push(CompilerAction::Compile { code: rest })
                            }

                            let (argc, argv) = match pair.get_cdr() {
                                SchemeType::Pair(rest) => (rest.len()?, Some(rest)),
                                SchemeType::EmptyList => (0, None),
                                _ => return Err(CompilerError::SyntaxError),
                            };
                            let function_name = SchemePair::one(pair.get_car());

                            stack.push(CompilerAction::EmitAsm {
                                statements: vec![Statement {
                                    s_type: StatementType::Call,
                                    arg: argc as u32,
                                }],
                            });

                            stack.push(CompilerAction::Compile {
                                code: function_name,
                            });

                            if let Some(arguments) = argv {
                                stack.push(CompilerAction::Compile { code: arguments });
                            }

                            continue 'stack_loop;
                        }
                        SchemeType::Symbol(ident_name) => {
                            let ident_or_macro = function.lookup(&ident_name)?;

                            if let CompilerType::Runtime(ident) = ident_or_macro {
                                current_code_block.push(Statement {
                                    s_type: StatementType::Get,
                                    arg: ident,
                                })
                            } else {
                                return Err(CompilerError::SyntaxError);
                            }
                        }
                        _ => {
                            current_code_block.push(Statement {
                                s_type: StatementType::Literal,
                                arg: function.compiled_code.literals.len() as u32,
                            });
                            function.compiled_code.literals.push(expr);
                        }
                    }
                }
            }
            CompilerAction::EmitAsm { mut statements } => {
                current_code_block.append(&mut statements);
            }
            CompilerAction::ExprDone => {
                let expr_list = replace(&mut current_code_block, Vec::new());
                stack.push(CompilerAction::EmitAsm {
                    statements: expr_list,
                })
            }
            CompilerAction::FunctionDone => {
                replace(&mut function.compiled_code.code, current_code_block);
                current_code_block = Vec::new();
                if let Some(parent) = function.parent {
                    function = *parent;
                } else {
                    break 'stack_loop;
                }
            }
        }
    }
    Ok(function.compiled_code)
}
