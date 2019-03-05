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
use crate::ast::{AstList, AstListBuilder, AstNode, AstSymbol, CoreSymbol, ListTerminator};
use crate::interperter::vm::{SchemeFunction, Statement, StatementType};
use std::mem::replace;

fn get_args(
    in_args: AstList,
    required_args: usize,
    optional_args: usize,
    vargs: bool,
) -> Result<(Vec<AstNode>, AstList), CompilerError> {
    let mut ret = Vec::new();
    let mut arg_iter = in_args.iter();

    for _ in 0..required_args {
        if let Some(x) = arg_iter.next() {
            ret.push(x.clone())
        } else {
            return Err(CompilerError::SyntaxError);
        }
    }

    for _ in 0..optional_args {
        if let Some(x) = arg_iter.next() {
            ret.push(x.clone())
        } else {
            break;
        }
    }

    let rest: AstList = arg_iter.collect();

    if !vargs && (!rest.is_empty_list() || in_args.is_improper_list()) {
        return Err(CompilerError::SyntaxError);
    }

    Ok((ret, rest))
}

#[derive(Clone)]
pub enum SchemeMacro {
    Builtin(BuiltinMacro),
}

impl SchemeMacro {
    pub fn expand(
        &self,
        args: AstList,
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
        in_args: AstList,
        function: &mut PartialFunction,
        state: CompilerState,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            BuiltinMacro::Lambda => {
                let (mut args, code) = get_args(in_args, 1, 0, true)?;

                if code.is_improper_list() {
                    return Err(CompilerError::SyntaxError);
                }

                let mut environment = EnvironmentFrame::new();

                let mut is_vargs = false;
                let raw_formals = args.pop().unwrap();
                let mut formal_len = 0;

                if let Some(formal_list) = raw_formals.to_list() {
                    for raw_formal in formal_list.iter() {
                        formal_len += 1;
                        let formal = if let Some(symbol) = raw_formal.to_symbol() {
                            symbol
                        } else {
                            return Err(CompilerError::SyntaxError);
                        };

                        environment.new_object(formal);
                    }

                    if formal_list.is_improper_list() {
                        let rest_name =
                            if let ListTerminator::Node(node) = formal_list.get_terminator() {
                                if let Some(name) = node.to_symbol() {
                                    name
                                } else {
                                    unreachable!();
                                }
                            } else {
                                return Err(CompilerError::SyntaxError);
                            };

                        is_vargs = true;

                        environment.new_object(rest_name);
                    }
                } else if let Some(formal_list) = raw_formals.to_symbol() {
                    is_vargs = true;

                    environment.new_object(formal_list);
                } else {
                    return Err(CompilerError::SyntaxError);
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

                if !code.is_empty_list() {
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
                    push_tail_body(code, &mut ret)?;
                    Ok(ret)
                } else {
                    Err(CompilerError::SyntaxError)
                }
            }
            BuiltinMacro::If => {
                let mut args = get_args(in_args, 2, 1, false)?.0;

                let false_expr = if args.len() == 3 {
                    args.pop().unwrap()
                } else {
                    AstList::one(CoreSymbol::GenUnspecified.into()).into()
                };

                let true_expr = args.pop().unwrap();
                let test = args.pop().unwrap();

                Ok(vec![
                    CompilerAction::IfCompileTrue {
                        true_expr: AstList::one(true_expr),
                        false_expr: AstList::one(false_expr),
                        state,
                    },
                    CompilerAction::Compile {
                        code: AstList::one(test),
                        state: CompilerState::Args,
                    },
                ])
            }
            BuiltinMacro::Set => {
                let mut args = get_args(in_args, 2, 0, false)?.0;

                let expr = args.pop().unwrap();
                let var = if let Some(x) = args.pop().unwrap().to_symbol() {
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
                        code: AstList::one(CoreSymbol::GenUnspecified.into()),
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
                    code: AstList::one(expr),
                    state: CompilerState::Args,
                });

                Ok(ret)
            }
            BuiltinMacro::Begin => {
                if in_args.is_improper_list() {
                    return Err(CompilerError::SyntaxError);
                }

                let code = [CoreSymbol::Let.into(), AstList::none().into()]
                    .iter()
                    .chain(in_args.iter())
                    .collect();
                Ok(vec![CompilerAction::Compile { code, state }])
            }
            BuiltinMacro::Quote => {
                let arg = get_args(in_args, 1, 0, false)?.0.pop().unwrap();

                if let CompilerState::Body = state {
                    Ok(Vec::new())
                } else {
                    let literal_n = function.compiled_code.literal_len();

                    function.compiled_code.new_literal(arg.to_datum());

                    Ok(vec![CompilerAction::EmitAsm {
                        statements: vec![Statement {
                            s_type: StatementType::Literal,
                            arg: literal_n as u32,
                        }],
                    }])
                }
            }
            BuiltinMacro::Let => {
                let (mut arg_list, code) = get_args(in_args, 1, 0, true)?;

                let definitions = if let Some(def) = arg_list.pop().unwrap().to_list() {
                    def
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let mut formals = AstListBuilder::new();
                let mut bindings = AstListBuilder::new();

                for definition_or_err in definitions.iter() {
                    let definition = if let Some(def) = definition_or_err.to_list() {
                        def
                    } else {
                        return Err(CompilerError::SyntaxError);
                    };

                    let mut def_list = get_args(definition, 2, 0, false)?.0;

                    bindings.push(def_list.pop().unwrap());
                    formals.push(def_list.pop().unwrap());
                }

                let lambda_def: AstList = [CoreSymbol::Lambda.into(), formals.build().into()]
                    .iter()
                    .chain(code.iter())
                    .collect();

                let ret_list: AstList = [lambda_def.into()]
                    .iter()
                    .chain(bindings.build().iter())
                    .collect();

                Ok(vec![CompilerAction::Compile {
                    code: AstList::one(ret_list.into()),
                    state,
                }])
            }
            BuiltinMacro::And => {
                let (mut arg_list, rest) = get_args(in_args, 0, 2, true)?;

                if rest.is_improper_list() {
                    return Err(CompilerError::SyntaxError);
                }

                let expr = if arg_list.is_empty() {
                    AstNode::from_bool(true)
                } else if arg_list.len() == 1 {
                    arg_list.pop().unwrap()
                } else {
                    let first_arg = arg_list.remove(0);

                    let and_list: AstList = [CoreSymbol::And.into(), arg_list.pop().unwrap()]
                        .iter()
                        .chain(rest.iter())
                        .collect();

                    let ret_list: AstList = [
                        CoreSymbol::If.into(),
                        first_arg,
                        and_list.into(),
                        AstNode::from_bool(false),
                    ]
                    .iter()
                    .collect();

                    ret_list.into()
                };

                Ok(vec![CompilerAction::Compile {
                    code: AstList::one(expr),
                    state,
                }])
            }
            BuiltinMacro::Or => {
                let (mut arg_list, rest) = get_args(in_args, 0, 2, true)?;

                let expr = if arg_list.is_empty() {
                    AstNode::from_bool(false)
                } else if arg_list.len() == 1 {
                    arg_list.pop().unwrap()
                } else {
                    let or_expanded_x = AstSymbol::gen_temp();

                    let binding_list: AstList = [or_expanded_x.clone().into(), arg_list.remove(0)]
                        .iter()
                        .collect();

                    let bindings = AstList::one(binding_list.into());

                    let or_list: AstList = [CoreSymbol::Or.into(), arg_list.pop().unwrap()]
                        .iter()
                        .chain(rest.iter())
                        .collect();

                    let if_list: AstList = [
                        CoreSymbol::If.into(),
                        or_expanded_x.clone().into(),
                        or_expanded_x.into(),
                        or_list.into(),
                    ]
                    .iter()
                    .collect();

                    let ret_list: AstList =
                        [CoreSymbol::Let.into(), bindings.into(), if_list.into()]
                            .iter()
                            .collect();

                    ret_list.into()
                };

                Ok(vec![CompilerAction::Compile {
                    code: AstList::one(expr),
                    state,
                }])
            }
        }
    }
}
