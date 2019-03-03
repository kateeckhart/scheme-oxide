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
    compiler::EnvironmentFrame, eval_with_environment, BuiltinFunction, FunctionRef,
    FunctionRefInner, RuntimeError,
};
use crate::ast::{AstSymbol, CoreSymbol};
use crate::types::*;
use std::cmp::Ordering;

pub struct BaseEnvironment {
    pub frame: EnvironmentFrame,
    pub bounded: Vec<SchemeType>,
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
        self.bounded.push(object)
    }

    fn push_builtin_function(&mut self, name: AstSymbol, function: BuiltinFunction) {
        self.push_object(
            name,
            SchemeType::Function(FunctionRef(FunctionRefInner::Builtin(function))),
        )
    }

    fn push_eval(&mut self, name: AstSymbol, expressions: &str) -> Result<(), RuntimeError> {
        let object = eval_with_environment(expressions, self)?;

        self.push_object(name, object);
        Ok(())
    }
}

fn gen_scheme_environment() -> BaseEnvironment {
    let mut ret = BaseEnvironment::new();

    ret.frame.add_builtin_macros();

    ret.push_builtin_function(AstSymbol::new("+"), BuiltinFunction::Add);
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
    ret.push_builtin_function(AstSymbol::new("car"), BuiltinFunction::Car);
    ret.push_builtin_function(AstSymbol::new("set_car!"), BuiltinFunction::SetCar);
    ret.push_builtin_function(AstSymbol::new("set_cdr!"), BuiltinFunction::SetCdr);
    ret.push_builtin_function(AstSymbol::new("cdr"), BuiltinFunction::Cdr);
    ret.push_builtin_function(AstSymbol::new("cons"), BuiltinFunction::Cons);

    ret.push_builtin_function(AstSymbol::new("eqv?"), BuiltinFunction::Eqv);
    ret.push_builtin_function(AstSymbol::new("quotient"), BuiltinFunction::Quotient);
    ret.push_builtin_function(AstSymbol::new("remainder"), BuiltinFunction::Remainder);

    ret.push_eval(AstSymbol::new("eq?"), "(lambda (x y) (eqv? x y))")
        .unwrap();
    ret.push_eval(AstSymbol::new("not"), "(lambda (x) (if x #f #t))")
        .unwrap();
    ret.push_eval(
        AstSymbol::new("boolean?"),
        "(lambda (x) (or (eqv? x #t) (eqv? x #f)))",
    )
    .unwrap();
    ret.push_eval(AstSymbol::new("null?"), "(lambda (x) (eqv? x (quote ())))")
        .unwrap();

    ret.push_eval(AstSymbol::new("zero?"), "(lambda (x) (= x 0))")
        .unwrap();
    ret.push_eval(AstSymbol::new("positive?"), "(lambda (x) (> x 0))")
        .unwrap();
    ret.push_eval(AstSymbol::new("negative?"), "(lambda (x) (< x 0))")
        .unwrap();

    ret.push_eval(
        AstSymbol::new("abs"),
        "(lambda (x) (if (negative? x) (- x) x))",
    )
    .unwrap();
    ret.push_eval(AstSymbol::new("list"), "(lambda list list)")
        .unwrap();

    ret.push_builtin_function(
        CoreSymbol::GenUnspecified.into(),
        BuiltinFunction::GenUnspecified,
    );
    ret.push_builtin_function(
        AstSymbol::new("$gen_unspecified"),
        BuiltinFunction::GenUnspecified,
    );

    ret
}

fn gen_main_environment() -> BaseEnvironment {
    gen_scheme_environment()
}

thread_local! {
    pub static SCHEME_ENVIORNMENT: BaseEnvironment = gen_scheme_environment();

    pub static MAIN_ENVIRONMENT: BaseEnvironment = gen_main_environment();
}
