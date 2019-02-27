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
use crate::interperter::{SchemeFunction, Statement, StatementType};
use crate::types::pair::{ListFactory, PairIterError};
use crate::types::*;
use std::mem::replace;

fn get_args(
    in_args: NullableSchemePair,
    required_args: usize,
    optional_args: usize,
    vargs: bool,
) -> Result<(Vec<SchemeType>, NullableSchemePair), CompilerError> {
    let mut ret = Vec::new();
    let mut arg_iter = in_args.iter();

    for _ in 0..required_args {
        if let Some(x) = arg_iter.next() {
            ret.push(x?)
        } else {
            return Err(CompilerError::SyntaxError);
        }
    }

    for _ in 0..optional_args {
        if let Some(x) = arg_iter.next() {
            ret.push(x?)
        } else {
            break;
        }
    }

    let rest = arg_iter.get_rest()?;

    if !vargs && rest.clone().into_option().is_some() {
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
        args: NullableSchemePair,
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
        in_args: NullableSchemePair,
        function: &mut PartialFunction,
        state: CompilerState,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            BuiltinMacro::Lambda => {
                let (mut args, code_or_none) = get_args(in_args, 1, 0, true)?;

                let mut environment = EnvironmentFrame::new();

                let mut is_vargs = false;
                let mut len = 0;
                let raw_formals = args.pop().unwrap();

                if let Ok(formal_pair) = raw_formals.to_nullable_pair() {
                    for raw_formal in formal_pair.iter() {
                        len += 1;
                        match raw_formal {
                            Ok(x) => {
                                environment.new_object(&x.to_symbol()?);
                            }
                            Err(PairIterError::Improper(rest)) => {
                                is_vargs = true;

                                environment.new_object(&rest.to_symbol()?);
                                break;
                            }
                            Err(err) => return Err(err.into()),
                        }
                    }
                } else if let Ok(formal_list) = raw_formals.to_symbol() {
                    is_vargs = true;

                    environment.new_object(&formal_list);
                }

                let parent = replace(
                    function,
                    PartialFunction {
                        compiled_code: SchemeFunction::new(len, is_vargs),
                        environment,
                        parent: None,
                    },
                );

                let lamada_n = parent.compiled_code.lamadas.len();

                function.parent = Some(Box::new(parent));

                if let Some(code) = code_or_none.into_option() {
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

                let false_expr = if args.len() >= 3 {
                    args.pop().unwrap()
                } else {
                    SchemePair::one(SchemeType::Symbol("$gen_unspecified".to_string())).into()
                };

                let true_expr = args.pop().unwrap();
                let test = args.pop().unwrap();

                Ok(vec![
                    CompilerAction::IfCompileTrue {
                        true_expr: SchemePair::one(true_expr),
                        false_expr: SchemePair::one(false_expr),
                        state,
                    },
                    CompilerAction::Compile {
                        code: SchemePair::one(test),
                        state: CompilerState::Args,
                    },
                ])
            }
            BuiltinMacro::Set => {
                let mut args = get_args(in_args, 2, 0, false)?.0;

                let expr = args.pop().unwrap();
                let var = args.pop().unwrap().to_symbol()?;

                let var_id = if let CompilerType::Runtime(x) = function.lookup(&var)? {
                    x
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let mut ret = Vec::new();

                if let CompilerState::Body = state {
                } else {
                    ret.push(CompilerAction::Compile {
                        code: SchemePair::one(SchemeType::Symbol("$gen_unspecified".to_string())),
                        state,
                    });
                }

                Ok(vec![
                    CompilerAction::EmitAsm {
                        statements: vec![Statement {
                            s_type: StatementType::Set,
                            arg: var_id,
                        }],
                    },
                    CompilerAction::Compile {
                        code: SchemePair::one(expr),
                        state: CompilerState::Args,
                    },
                ])
            }
            BuiltinMacro::Begin => {
                let mut ret = ListFactory::new();
                ret.push(SchemeType::Symbol("let".to_string()));
                ret.push(SchemeType::EmptyList);

                Ok(vec![CompilerAction::Compile {
                    code: SchemePair::one(ret.build_with_tail(in_args.into()).into()),
                    state,
                }])
            }
            BuiltinMacro::Quote => {
                let arg = get_args(in_args, 1, 0, false)?.0.pop().unwrap();

                if let CompilerState::Body = state {
                    Ok(Vec::new())
                } else {
                    let literal_n = function.compiled_code.literals.len();

                    function.compiled_code.literals.push(arg);

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

                let definitions = arg_list.pop().unwrap().to_nullable_pair()?;

                let mut lambda_def = ListFactory::new();
                lambda_def.push(SchemeType::Symbol("lambda".to_string()));

                let mut formals = Vec::new();
                let mut bindings = Vec::new();

                for definition in definitions.iter() {
                    let mut def_list = get_args(definition?.to_nullable_pair()?, 2, 0, false)?.0;

                    bindings.push(def_list.pop().unwrap());
                    formals.push(def_list.pop().unwrap());
                }

                let mut formal_list = ListFactory::new();

                for formal in formals {
                    formal_list.push(formal)
                }

                lambda_def.push(formal_list.build().into());

                let mut ret_list = ListFactory::new();

                ret_list.push(lambda_def.build_with_tail(code.into()).into());

                for binding in bindings {
                    ret_list.push(binding)
                }

                Ok(vec![CompilerAction::Compile {
                    code: SchemePair::one(ret_list.build().into()),
                    state,
                }])
            }
            BuiltinMacro::And => {
                let (mut arg_list, rest) = get_args(in_args, 0, 2, true)?;

                let expr = if arg_list.is_empty() {
                    SchemeType::Bool(true)
                } else if arg_list.len() == 1 {
                    arg_list.pop().unwrap()
                } else {
                    let mut ret_list = ListFactory::new();

                    ret_list.push(SchemeType::Symbol("if".to_string()));

                    ret_list.push(arg_list.remove(0));

                    let mut and_list = ListFactory::new();

                    and_list.push(SchemeType::Symbol("and".to_string()));

                    and_list.push(arg_list.pop().unwrap());

                    ret_list.push(and_list.build_with_tail(rest.into()).into());

                    ret_list.push(SchemeType::Bool(false));

                    ret_list.build().into()
                };

                Ok(vec![CompilerAction::Compile {
                    code: SchemePair::one(expr),
                    state,
                }])
            }
            BuiltinMacro::Or => {
                let (mut arg_list, rest) = get_args(in_args, 0, 2, true)?;

                let expr = if arg_list.is_empty() {
                    SchemeType::Bool(false)
                } else if arg_list.len() == 1 {
                    arg_list.pop().unwrap()
                } else {
                    let mut ret_list = ListFactory::new();

                    ret_list.push(SchemeType::Symbol("let".to_string()));

                    let mut binding_list = ListFactory::new();

                    binding_list.push(SchemeType::Symbol("$or$expanded$x".to_string()));
                    binding_list.push(arg_list.remove(0));

                    let bindings = SchemePair::one(binding_list.build().into());

                    ret_list.push(bindings.into());

                    let mut if_list = ListFactory::new();

                    if_list.push(SchemeType::Symbol("if".to_string()));
                    if_list.push(SchemeType::Symbol("$or$expanded$x".to_string()));
                    if_list.push(SchemeType::Symbol("$or$expanded$x".to_string()));

                    let mut or_list = ListFactory::new();

                    or_list.push(SchemeType::Symbol("or".to_string()));
                    or_list.push(arg_list.pop().unwrap());

                    if_list.push(or_list.build_with_tail(rest.into()).into());

                    ret_list.push(if_list.build().into());

                    ret_list.build().into()
                };

                Ok(vec![CompilerAction::Compile {
                    code: SchemePair::one(expr),
                    state,
                }])
            }
        }
    }
}
