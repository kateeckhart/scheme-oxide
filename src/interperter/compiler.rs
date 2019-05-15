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

use crate::ast::{AstListBuilder, AstNode, AstSymbol, CoreSymbol};
use crate::interperter::vm::{SchemeFunction, Statement, StatementType};
use std::collections::HashMap;
use std::mem::replace;

mod s_macro;
use self::s_macro::BuiltinMacro;

mod compiler_type;
use self::compiler_type::CompilerType;

mod error;
use self::error::AstCastErrorImpl;
pub use self::error::CompilerError;

fn compile_one<T>(node: AstNode, state: CompilerState) -> Result<Vec<CompilerAction>, T> {
    Ok(vec![CompilerAction::Compile { expr: node, state }])
}

pub fn parse_define(mut define: Vec<AstNode>) -> Result<(AstSymbol, AstNode), CompilerError> {
    if define.len() < 2 {
        return Err(CompilerError::argc("define", "2 or more", define.len()));
    }

    let define_name = define.remove(0);

    if define_name.as_symbol().is_some() && define.len() == 1 {
        let expr = define.pop().unwrap();
        Ok((define_name.into_symbol().unwrap(), expr))
    } else if define_name.as_list().is_some() {
        let (mut raw_formal_list, end) = define_name.into_list().unwrap().into_inner();
        if raw_formal_list.is_empty() {
            return Err(CompilerError::argc("define", "1 or more", define.len()));
        }

        let name = raw_formal_list
            .remove(0)
            .into_symbol()
            .into_compiler_result("define")?;

        let formals = if raw_formal_list.is_empty() {
            end
        } else {
            let mut formal_list = AstListBuilder::new();
            for formal in raw_formal_list {
                formal_list.push(formal)
            }

            formal_list.build_with_tail(end).unwrap().into()
        };

        let mut lambda_list = vec![CoreSymbol::Lambda.into(), formals];
        lambda_list.append(&mut define);

        Ok((name, lambda_list.into()))
    } else {
        Err(CompilerError::syntax("Invalid define form."))
    }
}

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
        self.push_builtin_macro(CoreSymbol::Quote.into(), BuiltinMacro::Quote);
        self.push_builtin_macro(AstSymbol::new("cond"), BuiltinMacro::Cond);
        self.push_builtin_macro(AstSymbol::new("letrec"), BuiltinMacro::LetRec);
        self.push_builtin_macro(CoreSymbol::LetRec.into(), BuiltinMacro::LetRec);
        self.push_builtin_macro(CoreSymbol::BeginProgram.into(), BuiltinMacro::BeginProgram);
    }

    fn push_builtin_macro(&mut self, name: AstSymbol, s_macro: BuiltinMacro) {
        self.map.insert(name, CompilerType::BuiltinMacro(s_macro));
    }

    fn lookup(&self, name: &AstSymbol) -> Option<CompilerType> {
        self.map.get(name).cloned()
    }
}

fn gen_tail_body(mut code: Vec<AstNode>) -> Result<Vec<CompilerAction>, CompilerError> {
    if code.is_empty() {
        return Err(CompilerError::syntax(
            "Tried to compile an empty body expression.",
        ));
    }

    let tail = code.pop().unwrap();

    let mut stack = vec![CompilerAction::Compile {
        expr: tail,
        state: CompilerState::Tail,
    }];

    stack.extend(code.into_iter().rev().map(|expr| CompilerAction::Compile {
        expr,
        state: CompilerState::Body,
    }));

    Ok(stack)
}

pub struct PartialFunction {
    compiled_code: SchemeFunction,
    environment: EnvironmentFrame,
    parent: Option<Box<PartialFunction>>,
}

impl PartialFunction {
    fn traverse_macro(&self, name: &AstSymbol) -> Result<CompilerType, CompilerError> {
        let mut function = Some(self);

        while let Some(func) = function {
            if let Some(compiler_type) = func.environment.lookup(name) {
                return Ok(compiler_type);
            } else {
                function = func.parent.as_ref().map(|x| &**x);
            }
        }

        Err(CompilerError::identifier_not_found(&name.get_name()))
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

            current_scope_or_none = current_scope.parent.as_ref().map(|x| &**x);
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

struct LetDef {
    formal: AstSymbol,
    binding: AstNode,
}

impl LetDef {
    fn from_raw_let(raw_defs: Vec<AstNode>) -> Result<Vec<LetDef>, CompilerError> {
        let mut defs = Vec::new();

        for definition_or_err in raw_defs {
            let bad_list_err = "Each let binding must be a proper list of 2.";

            let mut definition = if let Ok(def) = definition_or_err.into_proper_list() {
                def
            } else {
                return Err(CompilerError::syntax(bad_list_err));
            };

            if definition.len() != 2 {
                return Err(CompilerError::syntax(bad_list_err));
            }

            let binding = definition.pop().unwrap();
            let formal = definition
                .pop()
                .unwrap()
                .into_symbol()
                .into_compiler_result("Let")?;

            defs.push(LetDef { formal, binding })
        }

        Ok(defs)
    }
}

#[derive(Debug)]
pub struct LambdaBuilder {
    actions: Vec<CompilerAction>,
    args: Vec<AstSymbol>,
    vargs: Option<AstSymbol>,
    macros: Vec<(AstSymbol, CompilerType)>,
    state: CompilerState,
}

impl LambdaBuilder {
    fn new(actions: Vec<CompilerAction>, state: CompilerState) -> Self {
        Self {
            actions,
            args: Vec::new(),
            vargs: None,
            macros: Vec::new(),
            state,
        }
    }

    fn from_body_exprs(body: Vec<AstNode>, state: CompilerState) -> Result<Self, CompilerError> {
        Ok(Self::new(gen_tail_body(body)?, state))
    }

    fn add_args<T>(&mut self, args: T)
    where
        T: IntoIterator<Item = AstSymbol>,
    {
        self.args.extend(args)
    }

    fn add_vargs(&mut self, vargs: AstSymbol) {
        self.vargs = Some(vargs)
    }

    fn add_macros<T>(&mut self, macros: T)
    where
        T: IntoIterator<Item = (AstSymbol, CompilerType)>,
    {
        self.macros.extend(macros)
    }

    fn build(
        mut self,
        function: &mut PartialFunction,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        let mut new_env = EnvironmentFrame::new();
        let arg_count = self.args.len() as u32;

        for arg in self.args {
            new_env.new_object(arg);
        }

        let is_vargs = if let Some(vargs) = self.vargs {
            new_env.new_object(vargs);
            true
        } else {
            false
        };

        for (name, s_macro) in self.macros {
            new_env.map.insert(name, s_macro);
        }

        let parent = replace(
            function,
            PartialFunction {
                compiled_code: SchemeFunction::new(arg_count, is_vargs),
                environment: new_env,
                parent: None,
            },
        );

        let lamada_n = parent.compiled_code.lambda_len();

        function.parent = Some(Box::new(parent));

        let mut ret = Vec::new();
        if let CompilerState::Body = self.state {
        } else {
            ret.push(CompilerAction::EmitAsm {
                statements: vec![Statement {
                    arg: lamada_n as u32,
                    s_type: StatementType::Lamada,
                }],
            })
        }
        ret.push(CompilerAction::FunctionDone);
        ret.append(&mut self.actions);
        Ok(ret)
    }

    fn build_with_call(
        mut self,
        bindings: Vec<AstNode>,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        if self.vargs.is_some() {
            assert!(self.args.len() <= bindings.len())
        } else {
            assert_eq!(self.args.len(), bindings.len())
        }

        let mut compile_actions = add_call(bindings, self.state);
        self.state = CompilerState::Args;

        compile_actions.push(CompilerAction::Lambda(self));

        Ok(compile_actions)
    }

    fn build_using_letdefs<T>(mut self, defs: T) -> Result<Vec<CompilerAction>, CompilerError>
    where
        T: IntoIterator<Item = LetDef>,
    {
        let (bindings, formals): (Vec<_>, Vec<_>) = defs
            .into_iter()
            .map(|def| (def.binding, def.formal))
            .unzip();
        self.add_args(formals);

        self.build_with_call(bindings)
    }
}

#[derive(Debug)]
pub enum CompilerAction {
    Compile {
        expr: AstNode,
        state: CompilerState,
    },
    FunctionDone,
    EmitAsm {
        statements: Vec<Statement>,
    },
    PrependAsm {
        statements: Vec<Statement>,
    },
    Lambda(LambdaBuilder),
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

fn add_call(argv: Vec<AstNode>, state: CompilerState) -> Vec<CompilerAction> {
    let argc = argv.len() as u32;

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

    let mut stack = vec![CompilerAction::EmitAsm { statements }];

    stack.extend(argv.into_iter().rev().map(|expr| CompilerAction::Compile {
        expr,
        state: CompilerState::Args,
    }));

    stack
}

pub fn compile_function(
    base_environment: &EnvironmentFrame,
    expr: AstNode,
) -> Result<SchemeFunction, CompilerError> {
    let mut stack = vec![
        CompilerAction::FunctionDone,
        CompilerAction::Compile {
            expr,
            state: CompilerState::Tail,
        },
    ];

    let mut function = PartialFunction {
        compiled_code: SchemeFunction::default(),
        environment: base_environment.clone(),
        parent: None,
    };

    let mut current_code_block = Vec::new();

    while let Some(action) = stack.pop() {
        match action {
            CompilerAction::Compile { expr, state } => {
                //Function call/Macro use
                let parsed_expr = expr
                    .into_proper_list()
                    .map(|mut argv| {
                        let function_object = if !argv.is_empty() {
                            argv.remove(0)
                        } else {
                            return Err(CompilerError::syntax("Tried to call the empty list."));
                        };

                        let calling_function;
                        let mut expand_as_fn = None;

                        //If the name is a macro, expand the macro
                        if let Some(function_name) = function_object.as_symbol() {
                            calling_function = function.lookup(function_name)?;
                            expand_as_fn = calling_function.get_expand_as_fn_fn();
                        }

                        if let Some(expand_as_fn) = expand_as_fn {
                            stack.append(&mut expand_as_fn(argv, &mut function, state)?);
                        } else {
                            stack.append(&mut add_call(argv, state));

                            //Compile expression that evaluates to the function
                            stack.push(CompilerAction::Compile {
                                expr: function_object,
                                state: CompilerState::Args,
                            });
                        }

                        Ok(())
                    })
                    .or_else(|expr| {
                        expr.into_symbol().map(|ident_name| {
                            let ident = function.lookup(&ident_name)?;
                            let expand_as_self_or_none = ident.get_expand_as_self_fn();
                            if let Some(expand_as_self) = expand_as_self_or_none {
                                stack.append(&mut expand_as_self(
                                    &ident_name,
                                    &mut function,
                                    state,
                                )?);
                                Ok(())
                            } else {
                                Err(CompilerError::syntax(&format!(
                                    "Tried to get the value of the macro {}.",
                                    ident_name.get_name()
                                )))
                            }
                        })
                    })
                    .unwrap_or_else(|expr| {
                        if expr.is_improper_list() {
                            Err(CompilerError::syntax("Tried to call an improper list."))
                        } else {
                            if let CompilerState::Body = state {
                            } else {
                                current_code_block.push(Statement {
                                    s_type: StatementType::Literal,
                                    arg: function.compiled_code.literal_len() as u32,
                                });
                                function.compiled_code.new_literal(expr.to_datum());
                            }
                            Ok(())
                        }
                    });

                parsed_expr?
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
                }
            }
            CompilerAction::Lambda(builder) => {
                let code = current_code_block;
                current_code_block = Vec::new();

                stack.push(CompilerAction::PrependAsm { statements: code });
                stack.append(&mut builder.build(&mut function)?)
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
                    expr: true_expr,
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
                    expr: false_expr,
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
