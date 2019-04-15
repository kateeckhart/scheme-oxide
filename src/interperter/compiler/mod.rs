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

use crate::ast::{AstNode, AstSymbol, CoreSymbol};
use crate::interperter::vm::{SchemeFunction, Statement, StatementType};
use crate::types::*;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::vec;

mod s_macro;
use self::s_macro::BuiltinMacro;

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
        self.map
            .insert(name, CompilerType::RuntimeLocation(self.next_id));
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn add_builtin_macros(&mut self) {
        self.push_builtin_macro(AstSymbol::new("lambda"), BuiltinMacro::Lambda);
        self.push_builtin_macro(CoreSymbol::Lambda.into(), BuiltinMacro::Lambda);
        self.push_builtin_macro(AstSymbol::new("if"), BuiltinMacro::If);
        self.push_builtin_macro(CoreSymbol::If.into(), BuiltinMacro::If);
        self.push_builtin_macro(AstSymbol::new("let"), BuiltinMacro::Let);
        self.push_builtin_macro(CoreSymbol::Let.into(), BuiltinMacro::Let);
        self.push_builtin_macro(AstSymbol::new("let*"), BuiltinMacro::LetStar);
        self.push_builtin_macro(CoreSymbol::LetStar.into(), BuiltinMacro::LetStar);
        self.push_builtin_macro(AstSymbol::new("begin"), BuiltinMacro::Begin);
        self.push_builtin_macro(CoreSymbol::Begin.into(), BuiltinMacro::Begin);
        self.push_builtin_macro(AstSymbol::new("set!"), BuiltinMacro::Set);
        self.push_builtin_macro(CoreSymbol::Set.into(), BuiltinMacro::Set);
        self.push_builtin_macro(AstSymbol::new("or"), BuiltinMacro::Or);
        self.push_builtin_macro(CoreSymbol::Or.into(), BuiltinMacro::Or);
        self.push_builtin_macro(AstSymbol::new("and"), BuiltinMacro::And);
        self.push_builtin_macro(CoreSymbol::And.into(), BuiltinMacro::And);
        self.push_builtin_macro(AstSymbol::new("quote"), BuiltinMacro::Quote);
        self.push_builtin_macro(AstSymbol::new("cond"), BuiltinMacro::Cond);
    }

    fn push_builtin_macro(&mut self, name: AstSymbol, s_macro: BuiltinMacro) {
        self.map.insert(name, CompilerType::BuiltinMacro(s_macro));
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

fn push_tail_body(
    mut code: Vec<AstNode>,
    stack: &mut Vec<CompilerAction>,
) -> Result<(), CompilerError> {
    if code.is_empty() {
        return Err(CompilerError::SyntaxError);
    }

    let tail = code.pop().unwrap();

    stack.push(CompilerAction::Compile {
        code: vec![tail].into_iter(),
        state: CompilerState::Tail,
    });

    if !code.is_empty() {
        stack.push(CompilerAction::Compile {
            code: code.into_iter(),
            state: CompilerState::Body,
        })
    }
    Ok(())
}

#[derive(Clone)]
enum CompilerType {
    RuntimeLocation(u32),
    BuiltinMacro(BuiltinMacro),
}

impl CompilerType {
    fn does_expand_as_fn(&self) -> bool {
        match self {
            CompilerType::BuiltinMacro(_) => true,
            _ => false,
        }
    }

    fn expand_as_fn(
        &self,
        args: Vec<AstNode>,
        function: &mut PartialFunction,
        state: CompilerState,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            CompilerType::BuiltinMacro(m) => m.expand(args, function, state),
            _ => panic!(),
        }
    }

    fn be_captured(self, name: &AstSymbol, in_function: &mut PartialFunction) -> Self {
        match self {
            CompilerType::RuntimeLocation(_) => {
                let ret = in_function.environment.len();
                let mut function = Some(in_function);

                loop {
                    let func = function.take().unwrap();
                    func.environment.new_object(name.clone());

                    if let Some(parent) = func.parent.as_mut() {
                        if let Some(compiler_type) = parent.environment.lookup(name) {
                            if let CompilerType::RuntimeLocation(ident) = compiler_type {
                                func.compiled_code.new_capture(ident);
                                return CompilerType::RuntimeLocation(ret);
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
            _ => self,
        }
    }
}

pub struct PartialFunction {
    compiled_code: SchemeFunction,
    environment: EnvironmentFrame,
    parent: Option<Box<PartialFunction>>,
}

impl PartialFunction {
    fn traverse_macro(&mut self, name: &AstSymbol) -> Result<CompilerType, CompilerError> {
        let mut function = Some(self);

        while let Some(func) = function {
            if let Some(compiler_type) = func.environment.lookup(name) {
                return Ok(compiler_type);
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
            let s_macro = self.traverse_macro(name)?;
            Ok(s_macro.be_captured(name, self))
        }
    }

    fn is_bounded(&self, name: &AstSymbol) -> bool {
        let mut current_scope_or_none = Some(self);

        while let Some(current_scope) = current_scope_or_none {
            if current_scope.environment.lookup(name).is_some() {
                return true;
            }

            current_scope_or_none = current_scope.parent.as_ref().map(|x| x.deref());
        }
        false
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
        code: vec::IntoIter<AstNode>,
        state: CompilerState,
    },
    FunctionDone,
    EmitAsm {
        statements: Vec<Statement>,
    },
    PrependAsm {
        statements: Vec<Statement>,
    },
    IfCompileTrue {
        true_expr: AstNode,
        false_expr: AstNode,
        state: CompilerState,
    },
    IfCompileFalse {
        test_asm: Vec<Statement>,
        false_expr: AstNode,
        state: CompilerState,
    },
    IfCompileDone {
        test_asm: Vec<Statement>,
        true_asm: Vec<Statement>,
    },
}

pub fn compile_function(
    base_environment: &EnvironmentFrame,
    code: Vec<AstNode>,
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
            CompilerAction::Compile { mut code, state } => {
                while let Some(expr) = code.next() {
                    //Function call/Macro use
                    let list_or_err = expr.into_proper_list();
                    let list_parsed_or_err = match list_or_err {
                        Ok(mut argv) => {
                            //Backup the rest of the expressions in this block
                            stack.push(CompilerAction::Compile { code, state });

                            let function_object = if !argv.is_empty() {
                                argv.remove(0)
                            } else {
                                return Err(CompilerError::SyntaxError);
                            };

                            //If the name is a macro, expand the macro
                            if let Some(function_name) = function_object.to_symbol() {
                                let calling_function = function.lookup(&function_name)?;
                                if calling_function.does_expand_as_fn() {
                                    let code = current_code_block;
                                    current_code_block = Vec::new();

                                    stack.push(CompilerAction::PrependAsm { statements: code });
                                    stack.append(&mut calling_function.expand_as_fn(
                                        argv,
                                        &mut function,
                                        state,
                                    )?);
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

                            let function_name = vec![function_object];

                            //Compile the arguments to the function
                            if !argv.is_empty() {
                                stack.push(CompilerAction::Compile {
                                    code: argv.into_iter(),
                                    state: CompilerState::Args,
                                });
                            }

                            //Compile expression that evaluates to the function
                            stack.push(CompilerAction::Compile {
                                code: function_name.into_iter(),
                                state: CompilerState::Args,
                            });

                            continue 'stack_loop;
                        }
                        Err(err) => Err(err),
                    };

                    let res = list_parsed_or_err.or_else(|expr| {
                        expr.into_symbol().map(|ident_name| {
                            let ident_or_macro = function.lookup(&ident_name)?;

                            if let CompilerType::RuntimeLocation(ident) = ident_or_macro {
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
                            Ok(())
                        })
                    });

                    match res {
                        Ok(Err(err)) => return Err(err),
                        Ok(Ok(_)) => (),
                        Err(expr) => {
                            if expr.is_improper_list() {
                                return Err(CompilerError::SyntaxError);
                            }

                            if let CompilerState::Body = state {
                            } else {
                                current_code_block.push(Statement {
                                    s_type: StatementType::Literal,
                                    arg: function.compiled_code.literal_len() as u32,
                                });
                                function.compiled_code.new_literal(expr.to_datum());
                            }
                        }
                    }
                }
            }
            CompilerAction::PrependAsm { mut statements } => {
                statements.append(&mut current_code_block);
                current_code_block = statements
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
                    code: vec![true_expr].into_iter(),
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
                    code: vec![false_expr].into_iter(),
                    state,
                });
                current_code_block = Vec::new();
            }
            CompilerAction::IfCompileDone {
                mut test_asm,
                mut true_asm,
            } => {
                let mut false_asm = current_code_block;
                true_asm.push(Statement {
                    s_type: StatementType::Branch,
                    arg: false_asm.len() as u32,
                });
                test_asm.push(Statement {
                    s_type: StatementType::BranchIfFalse,
                    arg: true_asm.len() as u32,
                });

                current_code_block = test_asm;
                current_code_block.append(&mut true_asm);
                current_code_block.append(&mut false_asm);
            }
        }
    }
    Ok(function.compiled_code)
}
