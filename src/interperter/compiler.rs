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
use std::ops::DerefMut;
use std::rc::Rc;

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

    pub fn add_builtin_macros(&mut self) {
        self.push_macro("lamada", SchemeMacro::Builtin(BuiltinMacro::Lamada));
        self.push_macro("if", SchemeMacro::Builtin(BuiltinMacro::If));
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

impl From<CastError> for CompilerError {
    fn from(_: CastError) -> Self {
        CompilerError::SyntaxError
    }
}

impl From<pair::PairIterError> for CompilerError {
    fn from(_: pair::PairIterError) -> Self {
        CompilerError::BadList
    }
}

fn generate_unspecified() -> SchemeType {
    SchemeType::Bool(false)
}

#[derive(Clone)]
enum SchemeMacro {
    Builtin(BuiltinMacro),
}

impl SchemeMacro {
    fn expand(
        &self,
        args: NullableSchemePair,
        function: &mut PartialFunction,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            SchemeMacro::Builtin(s_macro) => s_macro.expand(args, function),
        }
    }
}

#[derive(Clone)]
enum BuiltinMacro {
    Lamada,
    If,
}

impl BuiltinMacro {
    fn expand(
        &self,
        in_args: NullableSchemePair,
        function: &mut PartialFunction,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            BuiltinMacro::Lamada => {
                let args = if let Some(a) = in_args.into_option() {
                    a
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let raw_formals = args.get_car().to_nullable_pair()?;
                let mut environment = EnvironmentFrame::new();

                for raw_formal in raw_formals.iter() {
                    environment.new_object(&raw_formal?.to_symbol()?);
                }

                let code_or_none = args.get_cdr().to_nullable_pair()?;

                let parent = replace(
                    function,
                    PartialFunction {
                        compiled_code: SchemeFunction::new(raw_formals.len()? as u32, false),
                        environment,
                        parent: None,
                    },
                );

                let lamada_n = parent.compiled_code.lamadas.len();

                function.parent = Some(Box::new(parent));

                if let Some(code) = code_or_none.into_option() {
                    Ok(vec![
                        CompilerAction::EmitAsm {
                            statements: vec![Statement {
                                arg: lamada_n as u32,
                                s_type: StatementType::Lamada,
                            }],
                        },
                        CompilerAction::FunctionDone,
                        CompilerAction::Compile { code },
                    ])
                } else {
                    Err(CompilerError::SyntaxError)
                }
            }
            BuiltinMacro::If => {
                let mut arg_iter = in_args.iter();
                //Grab the two required params, the optional param, and the rest.
                let (test, true_expr, false_expr_or_none, rest) = if let (Some(x), Some(y), z, z1) = (
                    arg_iter.next(),
                    arg_iter.next(),
                    arg_iter.next(),
                    arg_iter.next(),
                ) {
                    (x?, y?, z, z1)
                } else {
                    return Err(CompilerError::SyntaxError);
                };
                if rest.is_some() {
                    return Err(CompilerError::SyntaxError);
                };

                let false_expr = if let Some(expr) = false_expr_or_none {
                    expr?
                } else {
                    generate_unspecified()
                };

                Ok(vec![
                    CompilerAction::IfCompileTrue {
                        true_expr: SchemePair::one(true_expr),
                        false_expr: SchemePair::one(false_expr),
                    },
                    CompilerAction::Compile {
                        code: SchemePair::one(test),
                    },
                ])
            }
        }
    }
}

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

impl PartialFunction {
    fn traverse_macro(&mut self, name: &str) -> Result<Option<SchemeMacro>, CompilerError> {
        let mut function = Some(self);

        while let Some(func) = function {
            if let Some(compiler_type) = func.environment.lookup(name) {
                return Ok(match compiler_type {
                    CompilerType::Runtime(_) => None,
                    CompilerType::Macro(s_macro) => Some(s_macro),
                });
            } else {
                function = func.parent.as_mut().map(Box::deref_mut);
            }
        }

        Err(CompilerError::IdentifierNotFound)
    }

    fn lookup(&mut self, name: &str) -> Result<CompilerType, CompilerError> {
        if let Some(ident) = self.environment.lookup(name) {
            //Simple case: Variable has already been declared/looked up
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

#[derive(Debug)]
enum CompilerAction {
    Compile {
        code: SchemePair,
    },
    FunctionDone,
    EmitAsm {
        statements: Vec<Statement>,
    },
    IfCompileTrue {
        true_expr: SchemePair,
        false_expr: SchemePair,
    },
    IfCompileFalse {
        test_asm: Vec<Statement>,
        false_expr: SchemePair,
    },
    IfCompileDone {
        test_asm: Vec<Statement>,
        true_asm: Vec<Statement>,
    },
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
                        //Function call/Macro use
                        SchemeType::Pair(pair) => {
                            //Backup the rest of the expressions in this block
                            if let Some(rest) = expr_iter.get_rest()? {
                                stack.push(CompilerAction::Compile { code: rest })
                            }

                            let function_object = pair.get_car();
                            let argv = pair.get_cdr().to_nullable_pair()?;
                            //If the name is a macro, expand the macro
                            if let SchemeType::Symbol(function_name) = function_object {
                                let calling_function = function.lookup(&function_name)?;
                                if let CompilerType::Macro(s_macro) = calling_function {
                                    let code = current_code_block;
                                    current_code_block = Vec::new();

                                    stack.push(CompilerAction::EmitAsm { statements: code });
                                    stack.append(&mut s_macro.expand(argv, &mut function)?);
                                    continue 'stack_loop;
                                }
                            }

                            let argc = argv.len()?;

                            //Compile the call to the function
                            stack.push(CompilerAction::EmitAsm {
                                statements: vec![Statement {
                                    s_type: StatementType::Call,
                                    arg: argc as u32,
                                }],
                            });

                            let function_name = SchemePair::one(pair.get_car());

                            //Compile the arguments to the function
                            if let Some(arguments) = argv.into_option() {
                                stack.push(CompilerAction::Compile { code: arguments });
                            }

                            //Compile expression that evaluates to the function
                            stack.push(CompilerAction::Compile {
                                code: function_name,
                            });

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
            CompilerAction::FunctionDone => {
                replace(&mut function.compiled_code.code, current_code_block);
                current_code_block = Vec::new();
                if let Some(mut parent) = function.parent {
                    let child_code = function.compiled_code;
                    parent.compiled_code.lamadas.push(Rc::new(child_code));
                    function = *parent;
                } else {
                    break 'stack_loop;
                }
            }
            CompilerAction::IfCompileTrue {
                true_expr,
                false_expr,
            } => {
                stack.push(CompilerAction::IfCompileFalse {
                    false_expr,
                    test_asm: current_code_block,
                });
                stack.push(CompilerAction::Compile { code: true_expr });
                current_code_block = Vec::new();
            }
            CompilerAction::IfCompileFalse {
                false_expr,
                test_asm,
            } => {
                stack.push(CompilerAction::IfCompileDone {
                    test_asm,
                    true_asm: current_code_block,
                });
                stack.push(CompilerAction::Compile { code: false_expr });
                current_code_block = Vec::new();
            }
            CompilerAction::IfCompileDone {
                test_asm,
                mut true_asm,
            } => {
                let mut false_asm = current_code_block;
                current_code_block = test_asm;
                current_code_block.push(Statement {
                    s_type: StatementType::BranchIfFalse,
                    arg: true_asm.len() as u32 + 1,
                });
                current_code_block.append(&mut true_asm);
                current_code_block.push(Statement {
                    s_type: StatementType::Branch,
                    arg: false_asm.len() as u32,
                });
                current_code_block.append(&mut false_asm);
            }
        }
    }
    Ok(function.compiled_code)
}
