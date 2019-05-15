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
    compiler::parse_define, compiler::EnvironmentFrame, eval_with_environment, BuiltinFunction,
    FunctionRef, FunctionRefInner, RuntimeError,
};
use crate::ast::{AstNode, AstSymbol, CoreSymbol};
use crate::parser::Parser;
use crate::types::*;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

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
        let object = eval_with_environment(expression, self)?;

        self.push_object(name, object);
        Ok(())
    }
}

fn gen_scheme_environment() -> BaseEnvironment {
    let mut ret = BaseEnvironment::new();

    ret.frame.add_builtin_macros();

    ret.push_object(
        AstSymbol::new("$immutable-pair-type-id"),
        get_immutable_pair_type_id().into(),
    );
    ret.push_object(
        AstSymbol::new("$mutable-pair-type-id"),
        get_mutable_pair_type_id().into(),
    );
    ret.push_object(
        AstSymbol::new("$symbol-type-id"),
        get_symbol_type_id().into(),
    );

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

    let raw_std_lib = include_str!("../../scheme-src/std-lib.scm");
    let define_symbol = AstSymbol::new("define");
    for raw_statement in Parser::new(raw_std_lib) {
        let mut statement = raw_statement.unwrap().into_proper_list().unwrap();

        if let Some(true) = statement[0].as_symbol().map(|x| *x == define_symbol) {
            statement.remove(0);
            let (symbol, expr) = parse_define(statement).unwrap();
            ret.push_eval(symbol, expr).unwrap()
        } else {
            eval_with_environment(statement.into(), &ret).unwrap();
        }
    }

    ret
}

fn gen_main_environment() -> BaseEnvironment {
    gen_scheme_environment()
}

thread_local! {
    pub static SCHEME_ENVIORNMENT: BaseEnvironment = gen_scheme_environment();

    pub static MAIN_ENVIRONMENT: BaseEnvironment = gen_main_environment();
}
