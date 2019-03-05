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

use crate::ast::{AstList, AstListBuilder, AstNode, AstSymbol, CoreSymbol};
use crate::interperter::vm::{SchemeFunction, Statement, StatementType};
use crate::types::*;
use std::collections::HashMap;
use std::ops::DerefMut;

mod s_macro;
use self::s_macro::{BuiltinMacro, SchemeMacro};

#[derive(Clone)]
pub struct EnvironmentFrame {
    map: HashMap<AstSymbol, CompilerType>,
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

    pub fn new_object(&mut self, name: AstSymbol) -> u32 {
        self.map.insert(name, CompilerType::Runtime(self.next_id));
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn add_builtin_macros(&mut self) {
        self.push_macro(
            AstSymbol::new("lambda"),
            SchemeMacro::Builtin(BuiltinMacro::Lambda),
        );
        self.push_macro(
            CoreSymbol::Lambda.into(),
            SchemeMacro::Builtin(BuiltinMacro::Lambda),
        );
        self.push_macro(AstSymbol::new("if"), SchemeMacro::Builtin(BuiltinMacro::If));
        self.push_macro(
            CoreSymbol::If.into(),
            SchemeMacro::Builtin(BuiltinMacro::If),
        );
        self.push_macro(
            AstSymbol::new("let"),
            SchemeMacro::Builtin(BuiltinMacro::Let),
        );
        self.push_macro(
            CoreSymbol::Let.into(),
            SchemeMacro::Builtin(BuiltinMacro::Let),
        );
        self.push_macro(
            AstSymbol::new("begin"),
            SchemeMacro::Builtin(BuiltinMacro::Begin),
        );
        self.push_macro(
            AstSymbol::new("set!"),
            SchemeMacro::Builtin(BuiltinMacro::Set),
        );
        self.push_macro(AstSymbol::new("or"), SchemeMacro::Builtin(BuiltinMacro::Or));
        self.push_macro(
            CoreSymbol::Or.into(),
            SchemeMacro::Builtin(BuiltinMacro::Or),
        );
        self.push_macro(
            AstSymbol::new("and"),
            SchemeMacro::Builtin(BuiltinMacro::And),
        );
        self.push_macro(
            CoreSymbol::And.into(),
            SchemeMacro::Builtin(BuiltinMacro::And),
        );
        self.push_macro(
            AstSymbol::new("quote"),
            SchemeMacro::Builtin(BuiltinMacro::Quote),
        );
    }

    fn push_macro(&mut self, name: AstSymbol, s_macro: SchemeMacro) {
        self.map.insert(name, CompilerType::Macro(s_macro));
    }

    fn lookup(&self, name: &AstSymbol) -> Option<CompilerType> {
        self.map.get(name).cloned()
    }
}

#[derive(Debug)]
pub enum CompilerError {
    IdentifierNotFound,
    SyntaxError,
}

impl From<CastError> for CompilerError {
    fn from(_: CastError) -> Self {
        CompilerError::SyntaxError
    }
}

fn split_tail(list: AstList) -> Result<(AstList, AstNode), CompilerError> {
    if list.is_improper_list() {
        return Err(CompilerError::SyntaxError);
    }

    let mut factory = AstListBuilder::new();
    let mut iter = list.iter();

    let mut prev = iter.next().unwrap();

    for next in iter {
        factory.push(prev.clone());
        prev = next;
    }

    Ok((factory.build(), prev.clone()))
}

fn push_tail_body(code: AstList, stack: &mut Vec<CompilerAction>) -> Result<(), CompilerError> {
    let (body, tail) = split_tail(code)?;

    stack.push(CompilerAction::Compile {
        code: AstList::one(tail),
        state: CompilerState::Tail,
    });

    if !body.is_empty_list() {
        stack.push(CompilerAction::Compile {
            code: body,
            state: CompilerState::Body,
        })
    }
    Ok(())
}

#[derive(Clone)]
enum CompilerType {
    Runtime(u32),
    Macro(SchemeMacro),
}

pub struct PartialFunction {
    compiled_code: SchemeFunction,
    environment: EnvironmentFrame,
    parent: Option<Box<PartialFunction>>,
}

impl PartialFunction {
    fn traverse_macro(&mut self, name: &AstSymbol) -> Result<Option<SchemeMacro>, CompilerError> {
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

    fn lookup(&mut self, name: &AstSymbol) -> Result<CompilerType, CompilerError> {
        if let Some(ident) = self.environment.lookup(name) {
            //Simple case: Variable has already been declared/looked up
            return Ok(ident);
        } else {
            let macro_or_none = self.traverse_macro(name)?;

            if let Some(s_macro) = macro_or_none {
                self.environment.push_macro(name.clone(), s_macro.clone());
                return Ok(CompilerType::Macro(s_macro));
            } else {
                let ret = self.environment.len();
                let mut function = Some(self);

                loop {
                    let func = function.take().unwrap();
                    func.environment.new_object(name.clone());

                    if let Some(parent) = func.parent.as_mut() {
                        if let Some(compiler_type) = parent.environment.lookup(name) {
                            if let CompilerType::Runtime(ident) = compiler_type {
                                func.compiled_code.new_capture(ident);
                                return Ok(CompilerType::Runtime(ret));
                            } else {
                                unreachable!()
                            }
                        } else {
                            func.compiled_code.new_capture(parent.environment.len());
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

#[derive(Copy, Clone, Debug)]
pub enum CompilerState {
    Body,
    Tail,
    Args,
}

#[derive(Debug)]
pub enum CompilerAction {
    Compile {
        code: AstList,
        state: CompilerState,
    },
    FunctionDone,
    EmitAsm {
        statements: Vec<Statement>,
    },
    IfCompileTrue {
        true_expr: AstList,
        false_expr: AstList,
        state: CompilerState,
    },
    IfCompileFalse {
        test_asm: Vec<Statement>,
        false_expr: AstList,
        state: CompilerState,
    },
    IfCompileDone {
        test_asm: Vec<Statement>,
        true_asm: Vec<Statement>,
    },
}

pub fn compile_function(
    base_environment: &EnvironmentFrame,
    code: AstList,
) -> Result<SchemeFunction, CompilerError> {
    let mut stack = vec![CompilerAction::FunctionDone];

    push_tail_body(code, &mut stack)?;

    let mut function = PartialFunction {
        compiled_code: SchemeFunction::default(),
        environment: base_environment.clone(),
        parent: None,
    };

    let mut current_code_block = Vec::new();

    'stack_loop: while let Some(action) = stack.pop() {
        match action {
            CompilerAction::Compile { code, state } => {
                if code.is_improper_list() {
                    return Err(CompilerError::SyntaxError);
                }
                let mut expr_iter = code.iter();
                while let Some(expr) = expr_iter.next() {
                    //Function call/Macro use
                    if let Some(list) = expr.to_list() {
                        let rest: AstList = expr_iter.collect();

                        //Backup the rest of the expressions in this block
                        if !rest.is_empty_list() {
                            stack.push(CompilerAction::Compile { code: rest, state })
                        }

                        if list.is_improper_list() {
                            return Err(CompilerError::SyntaxError);
                        }

                        let mut list_iter = list.iter();

                        let function_object = if let Some(node) = list_iter.next() {
                            node
                        } else {
                            return Err(CompilerError::SyntaxError);
                        };

                        let argv: AstList = list_iter.collect();

                        //If the name is a macro, expand the macro
                        if let Some(function_name) = function_object.to_symbol() {
                            let calling_function = function.lookup(&function_name)?;
                            if let CompilerType::Macro(s_macro) = calling_function {
                                let code = current_code_block;
                                current_code_block = Vec::new();

                                stack.push(CompilerAction::EmitAsm { statements: code });
                                stack.append(&mut s_macro.expand(argv, &mut function, state)?);
                                continue 'stack_loop;
                            }
                        }

                        let argc = argv.len();

                        let s_type = if let CompilerState::Tail = state {
                            StatementType::Tail
                        } else {
                            StatementType::Call
                        };

                        //Compile the call to the function
                        let mut statements = vec![Statement {
                            s_type,
                            arg: argc as u32,
                        }];

                        if let CompilerState::Body = state {
                            statements.push(Statement {
                                s_type: StatementType::Discard,
                                arg: 0,
                            })
                        };

                        stack.push(CompilerAction::EmitAsm { statements });

                        let function_name = AstList::one(function_object.clone());

                        //Compile the arguments to the function
                        if !argv.is_empty_list() {
                            stack.push(CompilerAction::Compile {
                                code: argv,
                                state: CompilerState::Args,
                            });
                        }

                        //Compile expression that evaluates to the function
                        stack.push(CompilerAction::Compile {
                            code: function_name,
                            state: CompilerState::Args,
                        });

                        continue 'stack_loop;
                    } else if let Some(ident_name) = expr.to_symbol() {
                        let ident_or_macro = function.lookup(&ident_name)?;

                        if let CompilerType::Runtime(ident) = ident_or_macro {
                            if let CompilerState::Body = state {
                            } else {
                                current_code_block.push(Statement {
                                    s_type: StatementType::Get,
                                    arg: ident,
                                })
                            }
                        } else {
                            return Err(CompilerError::SyntaxError);
                        }
                    } else if let CompilerState::Body = state {
                    } else {
                        current_code_block.push(Statement {
                            s_type: StatementType::Literal,
                            arg: function.compiled_code.literal_len() as u32,
                        });
                        function.compiled_code.new_literal(expr.to_datum());
                    }
                }
            }
            CompilerAction::EmitAsm { mut statements } => {
                current_code_block.append(&mut statements);
            }
            CompilerAction::FunctionDone => {
                function.compiled_code.append_code(current_code_block);
                current_code_block = Vec::new();
                if let Some(mut parent) = function.parent {
                    let child_code = function.compiled_code;
                    parent.compiled_code.new_lambda(child_code);
                    function = *parent;
                } else {
                    break 'stack_loop;
                }
            }
            CompilerAction::IfCompileTrue {
                true_expr,
                false_expr,
                state,
            } => {
                stack.push(CompilerAction::IfCompileFalse {
                    false_expr,
                    test_asm: current_code_block,
                    state,
                });
                stack.push(CompilerAction::Compile {
                    code: true_expr,
                    state,
                });
                current_code_block = Vec::new();
            }
            CompilerAction::IfCompileFalse {
                false_expr,
                test_asm,
                state,
            } => {
                stack.push(CompilerAction::IfCompileDone {
                    test_asm,
                    true_asm: current_code_block,
                });
                stack.push(CompilerAction::Compile {
                    code: false_expr,
                    state,
                });
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
