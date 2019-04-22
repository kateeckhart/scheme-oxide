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
    compile_one, BuiltinMacro, CompilerAction, CompilerError, CompilerState, PartialFunction,
};
use crate::ast::{AstNode, AstSymbol, CoreSymbol};
use crate::interperter::vm::{Statement, StatementType};

#[derive(Clone, Debug)]
pub enum CompilerType {
    RuntimeLocation(u32),
    MaybeUndef { field: AstSymbol, is_def: AstSymbol },
    BuiltinMacro(BuiltinMacro),
}

impl CompilerType {
    pub fn get_expand_as_fn_fn(
        &self,
    ) -> Option<
        impl Fn(
                Vec<AstNode>,
                &mut PartialFunction,
                CompilerState,
            ) -> Result<Vec<CompilerAction>, CompilerError>
            + '_,
    > {
        match self {
            CompilerType::BuiltinMacro(_) => (),
            _ => return None,
        }

        Some(
            move |args, function: &mut PartialFunction, state| match self {
                CompilerType::BuiltinMacro(m) => m.expand(args, function, state),
                _ => unreachable!(),
            },
        )
    }

    pub fn get_expand_as_self_fn(
        &self,
    ) -> Option<
        impl Fn(
                &AstSymbol,
                &mut PartialFunction,
                CompilerState,
            ) -> Result<Vec<CompilerAction>, CompilerError>
            + '_,
    > {
        match self {
            CompilerType::RuntimeLocation(_) => (),
            CompilerType::MaybeUndef { .. } => (),
            _ => return None,
        }

        Some(
            move |name: &AstSymbol, _function: &mut PartialFunction, state| match self {
                CompilerType::RuntimeLocation(ident) => {
                    if let CompilerState::Body = state {
                        Ok(Vec::new())
                    } else {
                        Ok(vec![CompilerAction::EmitAsm {
                            statements: vec![Statement {
                                s_type: StatementType::Get,
                                arg: *ident,
                            }],
                        }])
                    }
                }
                CompilerType::MaybeUndef { field, is_def } => {
                    let quoted_name = vec![CoreSymbol::Quote.into(), name.clone().into()];
                    let error_list = vec![
                        CoreSymbol::Error.into(),
                        quoted_name.into(),
                        AstNode::from_string("Tried to read from undef.".to_string()),
                    ];
                    let if_list = vec![
                        CoreSymbol::If.into(),
                        is_def.clone().into(),
                        field.clone().into(),
                        error_list.into(),
                    ];

                    compile_one(if_list.into(), state)
                }
                _ => unreachable!(),
            },
        )
    }

    pub fn get_expand_as_set_fn(
        &self,
    ) -> Option<
        impl Fn(
                Vec<AstNode>,
                &mut PartialFunction,
                CompilerState,
            ) -> Result<Vec<CompilerAction>, CompilerError>
            + '_,
    > {
        match self {
            CompilerType::RuntimeLocation(_) => (),
            CompilerType::MaybeUndef { .. } => (),
            _ => return None,
        }

        Some(
            move |mut args: Vec<AstNode>, _function: &mut PartialFunction, state| match self {
                CompilerType::RuntimeLocation(var_id) => {
                    if args.len() != 1 {
                        return Err(CompilerError::SyntaxError);
                    }
                    let expr = args.pop().unwrap();
                    let mut ret = Vec::new();

                    if let CompilerState::Body = state {
                    } else {
                        ret.push(CompilerAction::Compile {
                            expr: CoreSymbol::GenUnspecified.into(),
                            state,
                        });
                    }

                    ret.push(CompilerAction::EmitAsm {
                        statements: vec![Statement {
                            s_type: StatementType::Set,
                            arg: *var_id,
                        }],
                    });

                    ret.push(CompilerAction::Compile {
                        expr,
                        state: CompilerState::Args,
                    });

                    Ok(ret)
                }
                CompilerType::MaybeUndef { field, is_def } => {
                    if args.len() != 1 {
                        return Err(CompilerError::SyntaxError);
                    }

                    let set_is_def = vec![
                        CoreSymbol::Set.into(),
                        is_def.clone().into(),
                        AstNode::from_bool(true),
                    ];
                    let set_field = vec![
                        CoreSymbol::Set.into(),
                        field.clone().into(),
                        args.pop().unwrap(),
                    ];

                    let begin_list = vec![
                        CoreSymbol::Begin.into(),
                        set_is_def.into(),
                        set_field.into(),
                    ];

                    compile_one(begin_list.into(), state)
                }
                _ => unreachable!(),
            },
        )
    }

    pub fn be_captured(self, name: &AstSymbol, in_function: &mut PartialFunction) -> Self {
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
            _ => {
                in_function
                    .environment
                    .map
                    .insert(name.clone(), self.clone());
                self
            }
        }
    }
}
