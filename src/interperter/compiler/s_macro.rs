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
    compile_one, CompilerAction, CompilerError, CompilerState, CompilerType, LambdaBuilder, LetDef,
    PartialFunction,
};
use crate::ast::{AstList, AstNode, AstSymbol, CoreSymbol};
use crate::interperter::vm::{Statement, StatementType};

#[derive(Clone, Debug)]
pub enum BuiltinMacro {
    Lambda,
    If,
    Set,
    Begin,
    Quote,
    //TODO: When syntax-rules is added, change into derived form.
    Let,
    LetStar,
    LetRec,
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

                let raw_formal_list = args.remove(0);
                let mut lambda_builder = LambdaBuilder::from_body_exprs(args, state)?;

                let parsed_res = raw_formal_list
                    .into_list()
                    .map(|formal_list| {
                        let formals_terminator = formal_list
                            .into_proper_list()
                            .map(|list| (list, None))
                            .or_else(|list| {
                                list.into_improper_list()
                                    .map(|list| (list.nodes, Some(list.terminator)))
                            });

                        let (raw_formals, terminator) = if let Ok(t) = formals_terminator {
                            t
                        } else {
                            return Err(CompilerError::SyntaxError);
                        };

                        for raw_formal in raw_formals {
                            let formal = if let Ok(symbol) = raw_formal.into_symbol() {
                                symbol
                            } else {
                                return Err(CompilerError::SyntaxError);
                            };

                            lambda_builder.add_args(Some(formal))
                        }

                        if let Some(rest) = terminator {
                            if let Ok(name) = rest.into_symbol() {
                                lambda_builder.add_vargs(name)
                            } else {
                                return Err(CompilerError::SyntaxError);
                            }
                        }

                        Ok(())
                    })
                    .or_else(|node| {
                        node.into_symbol().map(|formal_list| {
                            lambda_builder.add_vargs(formal_list);
                            Ok(())
                        })
                    });

                match parsed_res {
                    Ok(Ok(_)) => (),
                    Ok(Err(err)) => return Err(err),
                    Err(_) => return Err(CompilerError::SyntaxError),
                }

                Ok(vec![CompilerAction::Lambda(lambda_builder)])
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
                        expr: test,
                        state: CompilerState::Args,
                    },
                ])
            }
            BuiltinMacro::Set => {
                if args.is_empty() {
                    return Err(CompilerError::SyntaxError);
                }

                let var = if let Ok(x) = args.remove(0).into_symbol() {
                    x
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let compiler_type = function.lookup(&var)?;
                let expand_as_set_or_none = compiler_type.get_expand_as_set_fn();
                if let Some(expand_as_set) = expand_as_set_or_none {
                    expand_as_set(args, function, state)
                } else {
                    Err(CompilerError::SyntaxError)
                }
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

                let defs = LetDef::from_raw_let(definitions)?;

                match self_name {
                    Some(name) => {
                        let (mut bindings, formals): (Vec<_>, Vec<AstNode>) = defs
                            .into_iter()
                            .map(|def| (def.binding, def.formal.into()))
                            .unzip();

                        let mut lambda_def = vec![CoreSymbol::Lambda.into(), formals.into()];
                        lambda_def.append(&mut args);

                        let binding_list =
                            vec![vec![name.clone().into(), lambda_def.into()].into()].into();

                        let mut func_call = vec![name.clone().into()];
                        func_call.append(&mut bindings);

                        let outer_binding =
                            vec![CoreSymbol::LetRec.into(), binding_list, func_call.into()].into();

                        compile_one(outer_binding, state)
                    }
                    None => {
                        let lambda_builder = LambdaBuilder::from_body_exprs(args, state)?;
                        lambda_builder.build_using_letdefs(defs)
                    }
                }
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
            BuiltinMacro::LetRec => {
                if args.is_empty() {
                    return Err(CompilerError::SyntaxError);
                }

                let defs_or_err = args.remove(0).into_proper_list();

                let raw_defs = if let Ok(def) = defs_or_err {
                    def
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let in_defs = LetDef::from_raw_let(raw_defs)?;
                let mut undef_macros = Vec::new();
                let mut undef_defs = Vec::new();
                let mut list_of_sets = Vec::new();
                let mut tmp_bindings = Vec::new();

                for def in in_defs {
                    let undef_field = AstSymbol::gen_temp();
                    let is_def = AstSymbol::gen_temp();

                    let maybe_undef = CompilerType::MaybeUndef {
                        field: undef_field.clone(),
                        is_def: is_def.clone(),
                    };

                    undef_macros.push((def.formal.clone(), maybe_undef));

                    undef_defs.push(LetDef {
                        formal: undef_field,
                        binding: AstNode::from_bool(false),
                    });

                    undef_defs.push(LetDef {
                        formal: is_def,
                        binding: AstNode::from_bool(false),
                    });

                    let tmp_field = AstSymbol::gen_temp();
                    tmp_bindings.push(LetDef {
                        formal: tmp_field.clone(),
                        binding: def.binding,
                    });

                    list_of_sets.push(
                        vec![CoreSymbol::Set.into(), def.formal.into(), tmp_field.into()].into(),
                    );
                }

                let tmp_scope_builder =
                    LambdaBuilder::from_body_exprs(list_of_sets, CompilerState::Body)?;
                let mut tmp_scope = tmp_scope_builder.build_using_letdefs(tmp_bindings)?;

                let in_code_builder = LambdaBuilder::from_body_exprs(args, CompilerState::Tail)?;
                let in_code = in_code_builder.build_with_call(Vec::new())?;

                let mut outer_body = in_code;
                outer_body.append(&mut tmp_scope);

                let mut outer_scope_builder = LambdaBuilder::new(outer_body, state);
                outer_scope_builder.add_macros(undef_macros);
                outer_scope_builder.build_using_letdefs(undef_defs)
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
