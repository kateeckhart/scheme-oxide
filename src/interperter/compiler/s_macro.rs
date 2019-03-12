/*
    Copyright 2019 Alexander Eckhart

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

use super::{
    push_tail_body, CompilerAction, CompilerError, CompilerState, CompilerType, EnvironmentFrame,
    PartialFunction,
};
use crate::ast::{AstList, AstNode, AstSymbol, CoreSymbol};
use crate::interperter::vm::{SchemeFunction, Statement, StatementType};
use std::mem::replace;

#[derive(Clone)]
pub enum SchemeMacro {
    Builtin(BuiltinMacro),
}

impl SchemeMacro {
    pub fn expand(
        &self,
        args: Vec<AstNode>,
        function: &mut PartialFunction,
        state: CompilerState,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            SchemeMacro::Builtin(s_macro) => s_macro.expand(args, function, state),
        }
    }
}

#[derive(Clone)]
pub enum BuiltinMacro {
    Lambda,
    If,
    Set,
    Begin,
    Quote,
    //TODO: When syntax-rules is added, change into derived form.
    Let,
    Or,
    And,
}

impl BuiltinMacro {
    fn expand(
        &self,
        mut args: Vec<AstNode>,
        function: &mut PartialFunction,
        state: CompilerState,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            BuiltinMacro::Lambda => {
                if args.len() < 2 {
                    return Err(CompilerError::SyntaxError);
                }

                let mut environment = EnvironmentFrame::new();

                let mut is_vargs = false;

                let raw_formals = args.remove(0);
                let mut formal_len = 0;

                let parsed_res = raw_formals
                    .into_list()
                    .map(|formal_list| {
                        let formals_terminator = formal_list
                            .into_proper_list()
                            .map(|list| (list, None))
                            .or_else(|list| {
                                list.into_improper_list()
                                    .map(|list| (list.nodes, Some(list.terminator)))
                            });

                        let (formals, terminator) = if let Ok(t) = formals_terminator {
                            t
                        } else {
                            return Err(CompilerError::SyntaxError);
                        };

                        formal_len = formals.len() as u32;

                        for raw_formal in formals {
                            formal_len += 1;
                            let formal = if let Ok(symbol) = raw_formal.into_symbol() {
                                symbol
                            } else {
                                return Err(CompilerError::SyntaxError);
                            };

                            environment.new_object(formal);
                        }

                        if let Some(rest) = terminator {
                            if let Ok(name) = rest.into_symbol() {
                                is_vargs = true;

                                environment.new_object(name);
                            } else {
                                return Err(CompilerError::SyntaxError);
                            }
                        }

                        Ok(())
                    })
                    .or_else(|node| {
                        node.into_symbol().map(|formal_list| {
                            is_vargs = true;

                            environment.new_object(formal_list);
                            Ok(())
                        })
                    });

                match parsed_res {
                    Ok(Ok(_)) => (),
                    Ok(Err(err)) => return Err(err),
                    Err(_) => return Err(CompilerError::SyntaxError),
                }

                let parent = replace(
                    function,
                    PartialFunction {
                        compiled_code: SchemeFunction::new(formal_len, is_vargs),
                        environment,
                        parent: None,
                    },
                );

                let lamada_n = parent.compiled_code.lambda_len();

                function.parent = Some(Box::new(parent));

                let mut ret = Vec::new();
                if let CompilerState::Body = state {
                } else {
                    ret.push(CompilerAction::EmitAsm {
                        statements: vec![Statement {
                            arg: lamada_n as u32,
                            s_type: StatementType::Lamada,
                        }],
                    })
                }
                ret.push(CompilerAction::FunctionDone);
                push_tail_body(args, &mut ret)?;
                Ok(ret)
            }
            BuiltinMacro::If => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(CompilerError::SyntaxError);
                }

                let false_expr = if args.len() == 3 {
                    args.pop().unwrap()
                } else {
                    AstList::one(CoreSymbol::GenUnspecified.into()).into()
                };

                let true_expr = args.pop().unwrap();
                let test = args.pop().unwrap();

                Ok(vec![
                    CompilerAction::IfCompileTrue {
                        true_expr: vec![true_expr],
                        false_expr: vec![false_expr],
                        state,
                    },
                    CompilerAction::Compile {
                        code: vec![test].into_iter(),
                        state: CompilerState::Args,
                    },
                ])
            }
            BuiltinMacro::Set => {
                if args.len() != 2 {
                    return Err(CompilerError::SyntaxError);
                }

                let expr = args.pop().unwrap();
                let var = if let Ok(x) = args.pop().unwrap().into_symbol() {
                    x
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let var_id = if let CompilerType::Runtime(x) = function.lookup(&var)? {
                    x
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let mut ret = Vec::new();

                if let CompilerState::Body = state {
                } else {
                    ret.push(CompilerAction::Compile {
                        code: vec![CoreSymbol::GenUnspecified.into()].into_iter(),
                        state,
                    });
                }

                ret.push(CompilerAction::EmitAsm {
                    statements: vec![Statement {
                        s_type: StatementType::Set,
                        arg: var_id,
                    }],
                });

                ret.push(CompilerAction::Compile {
                    code: vec![expr].into_iter(),
                    state: CompilerState::Args,
                });

                Ok(ret)
            }
            BuiltinMacro::Begin => {
                let mut code = vec![CoreSymbol::Let.into(), AstList::none().into()];
                code.append(&mut args);

                Ok(vec![CompilerAction::Compile {
                    code: code.into_iter(),
                    state,
                }])
            }
            BuiltinMacro::Quote => {
                if args.len() != 1 {
                    return Err(CompilerError::SyntaxError);
                }

                if let CompilerState::Body = state {
                    Ok(Vec::new())
                } else {
                    let literal_n = function.compiled_code.literal_len();

                    function
                        .compiled_code
                        .new_literal(args.pop().unwrap().to_datum());

                    Ok(vec![CompilerAction::EmitAsm {
                        statements: vec![Statement {
                            s_type: StatementType::Literal,
                            arg: literal_n as u32,
                        }],
                    }])
                }
            }
            BuiltinMacro::Let => {
                if args.is_empty() {
                    return Err(CompilerError::SyntaxError);
                }

                let definitions = if let Ok(def) = args.remove(0).into_proper_list() {
                    def
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let mut formals = Vec::new();
                let mut bindings = Vec::new();

                for definition_or_err in definitions {
                    let mut definition = if let Ok(def) = definition_or_err.into_proper_list() {
                        def
                    } else {
                        return Err(CompilerError::SyntaxError);
                    };

                    if definition.len() != 2 {
                        return Err(CompilerError::SyntaxError);
                    }

                    bindings.push(definition.pop().unwrap());
                    formals.push(definition.pop().unwrap());
                }

                let mut lambda_def = vec![CoreSymbol::Lambda.into(), formals.into()];
                lambda_def.append(&mut args);

                let mut ret_list = vec![lambda_def.into()];
                ret_list.append(&mut bindings);

                Ok(vec![CompilerAction::Compile {
                    code: vec![ret_list.into()].into_iter(),
                    state,
                }])
            }
            BuiltinMacro::And => {
                let expr = if args.is_empty() {
                    AstNode::from_bool(true)
                } else if args.len() == 1 {
                    args.pop().unwrap()
                } else {
                    let first_arg = args.remove(0);

                    let mut and_list = vec![CoreSymbol::And.into()];
                    and_list.append(&mut args);

                    let ret_list = vec![
                        CoreSymbol::If.into(),
                        first_arg,
                        and_list.into(),
                        AstNode::from_bool(false),
                    ];

                    ret_list.into()
                };

                Ok(vec![CompilerAction::Compile {
                    code: vec![expr].into_iter(),
                    state,
                }])
            }
            BuiltinMacro::Or => {
                let expr = if args.is_empty() {
                    AstNode::from_bool(false)
                } else if args.len() == 1 {
                    args.pop().unwrap()
                } else {
                    let or_expanded_x = AstSymbol::gen_temp();

                    let binding_list = vec![or_expanded_x.clone().into(), args.remove(0)];

                    let bindings = vec![binding_list.into()];

                    let mut or_list = vec![CoreSymbol::Or.into()];
                    or_list.append(&mut args);

                    let if_list = vec![
                        CoreSymbol::If.into(),
                        or_expanded_x.clone().into(),
                        or_expanded_x.into(),
                        or_list.into(),
                    ];

                    let ret_list = vec![CoreSymbol::Let.into(), bindings.into(), if_list.into()];
                    ret_list.into()
                };

                Ok(vec![CompilerAction::Compile {
                    code: vec![expr].into_iter(),
                    state,
                }])
            }
        }
    }
}
