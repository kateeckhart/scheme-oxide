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
    generate_unspecified, CompilerAction, CompilerError, EnvironmentFrame, PartialFunction,
};
use crate::interperter::{SchemeFunction, Statement, StatementType};
use crate::types::*;
use std::mem::replace;

#[derive(Clone)]
pub enum SchemeMacro {
    Builtin(BuiltinMacro),
}

impl SchemeMacro {
    pub fn expand(
        &self,
        args: NullableSchemePair,
        function: &mut PartialFunction,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            SchemeMacro::Builtin(s_macro) => s_macro.expand(args, function),
        }
    }
}

#[derive(Clone)]
pub enum BuiltinMacro {
    Lamada,
    If,
}

impl BuiltinMacro {
    fn expand(
        &self,
        in_args: NullableSchemePair,
        function: &mut PartialFunction,
    ) -> Result<Vec<CompilerAction>, CompilerError> {
        match self {
            BuiltinMacro::Lamada => {
                let args = if let Some(a) = in_args.into_option() {
                    a
                } else {
                    return Err(CompilerError::SyntaxError);
                };

                let raw_formals = args.get_car().to_nullable_pair()?;
                let mut environment = EnvironmentFrame::new();

                for raw_formal in raw_formals.iter() {
                    environment.new_object(&raw_formal?.to_symbol()?);
                }

                let code_or_none = args.get_cdr().to_nullable_pair()?;

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
                    Ok(vec![
                        CompilerAction::EmitAsm {
                            statements: vec![Statement {
                                arg: lamada_n as u32,
                                s_type: StatementType::Lamada,
                            }],
                        },
                        CompilerAction::FunctionDone,
                        CompilerAction::Compile { code },
                    ])
                } else {
                    Err(CompilerError::SyntaxError)
                }
            }
            BuiltinMacro::If => {
                let mut arg_iter = in_args.iter();
                //Grab the two required params, the optional param, and the rest.
                let (test, true_expr, false_expr_or_none, rest) = if let (Some(x), Some(y), z, z1) = (
                    arg_iter.next(),
                    arg_iter.next(),
                    arg_iter.next(),
                    arg_iter.next(),
                ) {
                    (x?, y?, z, z1)
                } else {
                    return Err(CompilerError::SyntaxError);
                };
                if rest.is_some() {
                    return Err(CompilerError::SyntaxError);
                };

                let false_expr = if let Some(expr) = false_expr_or_none {
                    expr?
                } else {
                    generate_unspecified()
                };

                Ok(vec![
                    CompilerAction::IfCompileTrue {
                        true_expr: SchemePair::one(true_expr),
                        false_expr: SchemePair::one(false_expr),
                    },
                    CompilerAction::Compile {
                        code: SchemePair::one(test),
                    },
                ])
            }
        }
    }
}
