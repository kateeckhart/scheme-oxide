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

fn compile_one<T>(node: AstNode, state: CompilerState) -> Result<Vec<CompilerAction>, T> {
    Ok(vec![CompilerAction::Compile {
        code: vec![node].into_iter(),
        state,
    }])
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
    LetStar,
    Or,
    And,
    Cond,
}

impl BuiltinMacro {
    pub fn expand(
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
                        true_expr,
                        false_expr,
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

                let var_id = if let CompilerType::RuntimeLocation(x) = function.lookup(&var)? {
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

                compile_one(code.into(), state)
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

                let (self_name, definitions) = match args.remove(0).into_proper_list() {
                    Ok(def) => (None, def),
                    Err(node) => {
                        let self_name = if let Ok(sym) = node.into_symbol() {
                            sym
                        } else {
                            return Err(CompilerError::SyntaxError);
                        };

                        let defin = if let Ok(def) = args.remove(0).into_proper_list() {
                            def
                        } else {
                            return Err(CompilerError::SyntaxError);
                        };

                        (Some(self_name), defin)
                    }
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

                let expr = match self_name {
                    Some(name) => {
                        let set_list = vec![
                            CoreSymbol::Set.into(),
                            name.clone().into(),
                            lambda_def.into(),
                        ];

                        let mut func_call = vec![name.clone().into()];
                        func_call.append(&mut bindings);

                        let outer_binding = vec![name.into(), AstNode::from_bool(false)].into();
                        vec![
                            CoreSymbol::Let.into(),
                            vec![outer_binding].into(),
                            set_list.into(),
                            func_call.into(),
                        ]
                        .into()
                    }
                    None => {
                        let mut func_call = vec![lambda_def.into()];
                        func_call.append(&mut bindings);

                        func_call.into()
                    }
                };

                compile_one(expr, state)
            }
            BuiltinMacro::LetStar => {
                if args.len() < 2 {
                    return Err(CompilerError::SyntaxError);
                }

                let definitons_or_err = args.remove(0).into_proper_list();

                let mut definitions = if let Ok(def) = definitons_or_err {
                    def
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                if definitions.len() < 2 {
                    let mut let_list = vec![CoreSymbol::Let.into(), definitions.into()];
                    let_list.append(&mut args);

                    return compile_one(let_list.into(), state);
                }

                let def_one = definitions.remove(0);

                let mut let_list = vec![CoreSymbol::Let.into(), vec![def_one].into()];

                let mut code = vec![CoreSymbol::LetStar.into(), definitions.into()];
                code.append(&mut args);

                let_list.push(code.into());

                compile_one(let_list.into(), state)
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

                compile_one(expr, state)
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

                compile_one(expr, state)
            }
            BuiltinMacro::Cond => {
                if args.is_empty() {
                    return Err(CompilerError::SyntaxError);
                }

                let mut args_iter = args.into_iter().rev().peekable();
                let mut else_clase = vec![CoreSymbol::GenUnspecified.into()];

                let raw_borrowed_else_clase = args_iter.peek().unwrap();
                if let Some(clase) = raw_borrowed_else_clase.as_proper_list() {
                    let else_symbol = AstSymbol::new("else");
                    if !clase.is_empty()
                        && clase[0] == else_symbol.clone().into()
                        && !function.is_bounded(&else_symbol)
                    {
                        let mut raw_else_clase =
                            args_iter.next().unwrap().into_proper_list().unwrap();

                        raw_else_clase.remove(0);
                        else_clase = vec![CoreSymbol::Begin.into()];
                        else_clase.append(&mut raw_else_clase);
                    }
                }

                for raw_clase in args_iter {
                    let mut clase = if let Ok(clas) = raw_clase.into_proper_list() {
                        clas
                    } else {
                        return Err(CompilerError::SyntaxError);
                    };

                    if clase.is_empty() {
                        return Err(CompilerError::SyntaxError);
                    }

                    let test = clase.remove(0);

                    let mut begin = vec![CoreSymbol::Begin.into()];
                    begin.append(&mut clase);

                    let new_else_clase =
                        vec![CoreSymbol::If.into(), test, begin.into(), else_clase.into()];
                    else_clase = new_else_clase;
                }

                compile_one(else_clase.into(), state)
            }
        }
    }
}
