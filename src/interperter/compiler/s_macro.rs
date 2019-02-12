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
use crate::types::pair::ListFactory;
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
    //TODO: When syntax-rules is added, change into derived form.
    Let,
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
                let (args, code_or_none) = get_args(in_args, 1, 0, true)?;

                let raw_formals = args[0].to_nullable_pair()?;
                let mut environment = EnvironmentFrame::new();

                for raw_formal in raw_formals.iter() {
                    environment.new_object(&raw_formal?.to_symbol()?);
                }

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
                        code: SchemePair::one(SchemeType::Symbol("$gen_unspecified".to_string()))
                            .into(),
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
        }
    }
}
