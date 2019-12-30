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

use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

use crate::ast::{AstNode, AstSymbol, CoreSymbol};
use crate::parser::Parser;
use crate::types::*;

use super::{
    compiler::parse_define, compiler::EnvironmentFrame, eval_with_environment, BuiltinFunction,
    FunctionRef, FunctionRefInner, RuntimeError,
};

#[derive(Clone)]
pub struct BaseEnvironment {
    pub frame: EnvironmentFrame,
    pub bounded: Vec<Rc<RefCell<SchemeType>>>,
}

impl BaseEnvironment {
    fn new() -> Self {
        Self {
            frame: EnvironmentFrame::new(),
            bounded: Vec::new(),
        }
    }

    pub fn eval(&self, statement: AstNode) -> Result<SchemeType, RuntimeError> {
        eval_with_environment(statement, self)
    }

    pub fn eval_str(&self, string: &str) -> Result<SchemeType, RuntimeError> {
        let parsed_nodes: Result<Vec<_>, _> = Parser::new(string).collect();
        let nodes = vec![CoreSymbol::BeginProgram.into(), parsed_nodes?.into()];

        self.eval(nodes.into())
    }

    fn push_object(&mut self, name: AstSymbol, object: SchemeType) {
        self.frame.new_object(name);
        self.bounded.push(Rc::new(RefCell::new(object)))
    }

    fn push_builtin_function(&mut self, name: AstSymbol, function: BuiltinFunction) {
        self.push_object(
            name,
            SchemeType::Function(FunctionRef(FunctionRefInner::Builtin(function))),
        )
    }

    fn push_eval(&mut self, name: AstSymbol, expression: AstNode) -> Result<(), RuntimeError> {
        let object = self.eval(expression)?;

        self.push_object(name, object);
        Ok(())
    }

    fn push_lib_file(&mut self, file: &str) -> Result<(), RuntimeError> {
        let define_symbol = AstSymbol::new("define");
        for raw_statement in Parser::new(file) {
            let mut statement = raw_statement.unwrap().into_proper_list().unwrap();

            if let Some(true) = statement[0].as_symbol().map(|x| *x == define_symbol) {
                statement.remove(0);
                let (symbol, expr) = parse_define(statement)?;
                self.push_eval(symbol, expr)?
            } else {
                self.eval(statement.into())?;
            }
        }
        Ok(())
    }
}

fn gen_stage0_environment() -> BaseEnvironment {
    let mut ret = BaseEnvironment::new();

    ret.frame.add_stage0_macros();

    let newline_str: SchemeString = "\n".parse().unwrap();
    ret.push_object(AstSymbol::new("$newline-str"), newline_str.into());

    ret.push_builtin_function(AstSymbol::new("+"), BuiltinFunction::Add);
    ret.push_builtin_function(AstSymbol::new("*"), BuiltinFunction::Mul);
    ret.push_builtin_function(AstSymbol::new("-"), BuiltinFunction::Sub);

    ret.push_builtin_function(
        AstSymbol::new("="),
        BuiltinFunction::Compare {
            invert: false,
            mode: Ordering::Equal,
        },
    );
    ret.push_builtin_function(
        AstSymbol::new("<"),
        BuiltinFunction::Compare {
            invert: false,
            mode: Ordering::Less,
        },
    );
    ret.push_builtin_function(
        AstSymbol::new("<="),
        BuiltinFunction::Compare {
            invert: true,
            mode: Ordering::Greater,
        },
    );
    ret.push_builtin_function(
        AstSymbol::new(">"),
        BuiltinFunction::Compare {
            invert: false,
            mode: Ordering::Greater,
        },
    );
    ret.push_builtin_function(
        AstSymbol::new(">="),
        BuiltinFunction::Compare {
            invert: true,
            mode: Ordering::Less,
        },
    );
    ret.push_builtin_function(AstSymbol::new("$make-object"), BuiltinFunction::NewObject);
    ret.push_builtin_function(AstSymbol::new("$object?"), BuiltinFunction::IsObject);
    ret.push_builtin_function(
        AstSymbol::new("$object-type-id-get"),
        BuiltinFunction::GetTypeId,
    );
    ret.push_builtin_function(
        AstSymbol::new("$object-field-get"),
        BuiltinFunction::GetField,
    );
    ret.push_builtin_function(
        AstSymbol::new("$object-field-set!"),
        BuiltinFunction::SetField,
    );

    ret.push_builtin_function(AstSymbol::new("eqv?"), BuiltinFunction::Eqv);
    ret.push_builtin_function(AstSymbol::new("quotient"), BuiltinFunction::Quotient);
    ret.push_builtin_function(AstSymbol::new("remainder"), BuiltinFunction::Remainder);
    ret.push_builtin_function(AstSymbol::new("error"), BuiltinFunction::Error);
    ret.push_builtin_function(CoreSymbol::Error.into(), BuiltinFunction::Error);

    ret.push_builtin_function(
        CoreSymbol::GenUnspecified.into(),
        BuiltinFunction::GenUnspecified,
    );

    ret.push_builtin_function(AstSymbol::new("make-string"), BuiltinFunction::NewString);
    ret.push_builtin_function(AstSymbol::new("string-length"), BuiltinFunction::StringLen);
    ret.push_builtin_function(AstSymbol::new("string-ref"), BuiltinFunction::GetChar);
    ret.push_builtin_function(AstSymbol::new("string-set!"), BuiltinFunction::SetChar);
    ret.push_builtin_function(AstSymbol::new("number?"), BuiltinFunction::IsNumber);
    ret.push_builtin_function(AstSymbol::new("char?"), BuiltinFunction::IsChar);
    ret.push_builtin_function(AstSymbol::new("string?"), BuiltinFunction::IsString);
    ret.push_builtin_function(AstSymbol::new("write-char"), BuiltinFunction::WriteChar);

    ret
}

fn gen_stage1_environment() -> BaseEnvironment {
    let mut ret = gen_stage0_environment();

    ret.push_lib_file(include_str!("../../scheme-src/stage1.scm"))
        .unwrap();

    ret
}

fn gen_scheme_environment() -> BaseEnvironment {
    let mut ret = STAGE1_ENVIRONMENT.with(Clone::clone);

    ret.frame.add_stage2_macros();
    ret.push_lib_file(include_str!("../../scheme-src/std-lib.scm"))
        .unwrap();

    ret
}

thread_local! {
    pub static STAGE1_ENVIRONMENT: BaseEnvironment = gen_stage1_environment();

    pub static SCHEME_ENVIRONMENT: BaseEnvironment = gen_scheme_environment();
}
