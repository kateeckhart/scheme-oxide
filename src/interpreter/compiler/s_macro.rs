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

use crate::ast::{AstList, AstNode, AstSymbol, CoreSymbol};
use crate::interpreter::vm::{Statement, StatementType};

use super::{
    compile_one, CompilerAction, CompilerError, CompilerState, CompilerType,
    error::AstCastErrorImpl, LambdaBuilder, LetDef, PartialFunction,
};

#[derive(Clone, Debug)]
pub enum BuiltinMacro {
    Lambda { is_stage_1: bool },
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
    BeginProgram,
}

fn assert_args(
    what: &str,
    args: &[AstNode],
    argc: usize,
    is_vargs: bool,
) -> Result<(), CompilerError> {
    if (is_vargs && args.len() < argc) || (!is_vargs && args.len() != argc) {
        let or_more = if is_vargs { " or more" } else { "" };

        Err(CompilerError::argc(
            &format!("{}{}", argc, or_more),
            what,
            args.len(),
        ))
    } else {
        Ok(())
    }
}

impl BuiltinMacro {
    pub fn expand(
        &self,
        mut args: Vec<AstNode>,
        function: &mut PartialFunction,
        state: CompilerState,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            BuiltinMacro::Lambda { is_stage_1 } => {
                assert_args("lambda", &args, 2, true)?;

                let raw_formal_list = args.remove(0);
                let mut lambda_builder = LambdaBuilder::from_body_exprs(args, state)?;

                let parsed_res = raw_formal_list
                    .into_list()
                    .map(|formal_list| {
                        let (raw_formals, terminator) = formal_list.into_inner();

                        for raw_formal in raw_formals {
                            let formal = raw_formal.into_symbol().into_compiler_result("lambda")?;

                            lambda_builder.add_args(Some(formal))
                        }

                        if !terminator
                            .as_list()
                            .map(AstList::is_empty_list)
                            .unwrap_or(false)
                        {
                            if *is_stage_1 {
                                return Err(CompilerError::syntax(
                                    "Varargs do not work in stage1.",
                                ));
                            }

                            let name = terminator.into_symbol().into_compiler_result("lambda")?;

                            lambda_builder.add_vargs(name)
                        }

                        Ok(())
                    })
                    .or_else(|node| {
                        node.into_symbol().map(|formal_list| {
                            lambda_builder.add_vargs(formal_list);
                            if *is_stage_1 {
                                Err(CompilerError::syntax("Varargs do not work in stage1."))
                            } else {
                                Ok(())
                            }
                        })
                    });

                match parsed_res {
                    Ok(Ok(_)) => (),
                    Ok(Err(err)) => return Err(err),
                    Err(_) => return Err(CompilerError::syntax(
                        "Lambda expects either a proper or improper list for the formals argument.",
                    )),
                }

                Ok(vec![CompilerAction::Lambda(lambda_builder)])
            }
            BuiltinMacro::If => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(CompilerError::argc("if", "2 or 3", args.len()));
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
                assert_args("set!", &args, 1, true)?;

                let var = args.remove(0).into_symbol().into_compiler_result("set!")?;

                let compiler_type = function.lookup(&var)?;
                let expand_as_set_or_none = compiler_type.get_expand_as_set_fn();
                if let Some(expand_as_set) = expand_as_set_or_none {
                    expand_as_set(args, function, state)
                } else {
                    Err(CompilerError::syntax(&format!(
                        "Tried to set the macro {}.",
                        var.get_name()
                    )))
                }
            }
            BuiltinMacro::Begin => {
                let mut code = vec![CoreSymbol::Let.into(), AstList::none().into()];
                code.append(&mut args);

                compile_one(code.into(), state)
            }
            BuiltinMacro::Quote => {
                assert_args("quote", &args, 1, false)?;

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
                assert_args("let", &args, 2, true)?;

                let (self_name, definitions) = match args.remove(0).into_proper_list() {
                    Ok(def) => (None, def),
                    Err(node) => {
                        let self_name = if let Ok(sym) = node.into_symbol() {
                            sym
                        } else {
                            return Err(CompilerError::syntax("let expects either a proper list or a symbol as the first argument"));
                        };

                        let defin = if let Ok(def) = args.remove(0).into_proper_list() {
                            def
                        } else {
                            return Err(CompilerError::syntax("let expects a symbol followed by a proper list for its named let form."));
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
                assert_args("let*", &args, 2, true)?;

                let mut definitions = args
                    .remove(0)
                    .into_proper_list()
                    .into_compiler_result("let*")?;

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
                assert_args("letrec", &args, 1, true)?;

                let raw_defs = args
                    .remove(0)
                    .into_proper_list()
                    .into_compiler_result("letrec")?;

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
                assert_args("cond", &args, 1, true)?;

                let mut args_iter = args.into_iter().rev().peekable();
                let mut else_clause = vec![CoreSymbol::GenUnspecified.into()];

                let raw_borrowed_else_clause = args_iter.peek().unwrap();
                if let Some(clause) = raw_borrowed_else_clause.as_proper_list() {
                    let else_symbol = AstSymbol::new("else");
                    if !clause.is_empty()
                        && clause[0] == else_symbol.clone().into()
                        && !function.is_bounded(&else_symbol)
                    {
                        let mut raw_else_clause =
                            args_iter.next().unwrap().into_proper_list().unwrap();

                        raw_else_clause.remove(0);
                        else_clause = vec![CoreSymbol::Begin.into()];
                        else_clause.append(&mut raw_else_clause);
                    }
                }

                for raw_clause in args_iter {
                    let mut clause = raw_clause.into_proper_list().into_compiler_result("cond")?;

                    if clause.is_empty() {
                        return Err(CompilerError::syntax("Clause list cannot be empty."));
                    }

                    let test = clause.remove(0);

                    let new_else_clause = if clause.is_empty() {
                        let test_res = AstSymbol::gen_temp();
                        let bindings = vec![vec![test_res.clone().into(), test].into()];
                        let if_list = vec![
                            CoreSymbol::If.into(),
                            test_res.clone().into(),
                            test_res.into(),
                            else_clause.into(),
                        ];
                        vec![CoreSymbol::Let.into(), bindings.into(), if_list.into()]
                    } else {
                        let mut begin = vec![CoreSymbol::Begin.into()];
                        begin.append(&mut clause);
                        vec![
                            CoreSymbol::If.into(),
                            test,
                            begin.into(),
                            else_clause.into(),
                        ]
                    };

                    else_clause = new_else_clause;
                }

                compile_one(else_clause.into(), state)
            }
            BuiltinMacro::BeginProgram => {
                assert_args("$begin-program", &args, 1, false)?;

                let code = args
                    .pop()
                    .unwrap()
                    .into_proper_list()
                    .into_compiler_result("$begin-program")?;

                let lambda_builder = LambdaBuilder::from_body_exprs(code, state)?;

                lambda_builder.build_using_letdefs(function.environment.map.iter().filter_map(
                    |(var, value)| match value {
                        //Copy all runtime variables to prevent the derived forms that come with
                        //scheme-oxide from having undefined behavior if they are changed.
                        CompilerType::RuntimeLocation(_) => Some(LetDef {
                            formal: var.clone(),
                            binding: var.clone().into(),
                        }),
                        _ => None,
                    },
                ))
            }
        }
    }
}
