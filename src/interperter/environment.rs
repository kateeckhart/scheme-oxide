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

    fn push_eval(&mut self, name: AstSymbol, expressions: &str) -> Result<(), RuntimeError> {
        let object = eval_with_environment(Parser::new(expressions).next().unwrap()?, self)?;

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
    ret.push_builtin_function(AstSymbol::new("$object-field-get"), BuiltinFunction::GetField);
    ret.push_builtin_function(AstSymbol::new("$object-field-set!"), BuiltinFunction::SetField);

    ret.push_builtin_function(AstSymbol::new("eqv?"), BuiltinFunction::Eqv);
    ret.push_builtin_function(AstSymbol::new("quotient"), BuiltinFunction::Quotient);
    ret.push_builtin_function(AstSymbol::new("remainder"), BuiltinFunction::Remainder);
    ret.push_builtin_function(AstSymbol::new("error"), BuiltinFunction::Error);
    ret.push_builtin_function(CoreSymbol::Error.into(), BuiltinFunction::Error);

    ret.push_builtin_function(
        CoreSymbol::GenUnspecified.into(),
        BuiltinFunction::GenUnspecified,
    );
    ret.push_builtin_function(
        AstSymbol::new("$gen-unspecified"),
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

    ret.push_eval(AstSymbol::new("eq?"), "(lambda (x y) (eqv? x y))")
        .unwrap();
    ret.push_eval(AstSymbol::new("null?"), "(lambda (x) (eqv? x '()))")
        .unwrap();
    ret.push_eval(AstSymbol::new("not"), "(lambda (x) (if x #f #t))")
        .unwrap();
    ret.push_eval(
        AstSymbol::new("boolean?"),
        "(lambda (x) (or (eqv? x #t) (eqv? x #f)))",
    )
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

    ret.push_eval(
        AstSymbol::new("$mutable-pair?"),
        "(lambda (x) (and ($object? x) (eqv? ($object-type-id-get x) $mutable-pair-type-id)))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("pair?"),
        "(lambda (x) (or ($mutable-pair? x) (and ($object? x) (eqv? ($object-type-id-get x) $immutable-pair-type-id))))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("$assert-pair"),
        r#"(lambda (name x) (if (not (pair? x)) (error name "Not a pair." x)))"#,
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("$assert-mutable-pair"),
        r#"(lambda (name x) (if (not ($mutable-pair? x)) (error name "Not a mutable pair." x)))"#,
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("car"),
        "(lambda (x) ($assert-pair 'car x) ($object-field-get x 0))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("cdr"),
        "(lambda (x) ($assert-pair 'cdr x) ($object-field-get x 1))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("set-car!"),
        "(lambda (x y) ($assert-mutable-pair 'set-car! x) ($object-field-set! x 0 y))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("set-cdr!"),
        "(lambda (x y) ($assert-mutable-pair 'set-cdr! x) ($object-field-set! x 1 y))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("cons"),
        "(lambda (x y) ($make-object $mutable-pair-type-id x y))",
    )
    .unwrap();

    ret.push_eval(AstSymbol::new("equal?"), "
        (lambda (x y)
            (let equal? ((x x) (y y))
                (if (eqv? x y)
                    #t
                    (cond
                        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
                        (else #f)))))").unwrap();

    ret.push_eval(
        AstSymbol::new("max"),
        "
        (lambda (x . in-rest)
            (let max ((x x) (rest in-rest))
                (if (null? rest)
                    x
                    (let ((y (car rest)) (new-rest (cdr rest)))
                        (if (< x y)
                            (max y new-rest)
                            (max x new-rest))))))",
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("min"),
        "
        (lambda (x . in-rest)
            (let min ((x x) (rest in-rest))
                (if (null? rest)
                    x
                    (let ((y (car rest)) (new-rest (cdr rest)))
                        (if (< x y)
                            (min x new-rest)
                            (min y new-rest))))))",
    )
    .unwrap();

    ret.push_eval(
        AstSymbol::new("$string-copy-onto!"),
        r#"
        (lambda (src dest size)
            (if (or (> size (string-length src)) (> size (string-length dest)))
                (error '$string-copy-onto "Size is greater than length.")
                (let copy-onto ((index 0))
                    (if (= index size)
                        (if #f #f)
                        (let ((char (string-ref src index)))
                            (string-set! dest index char)
                            (copy-onto (+ index 1)))))))"#,
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("$string-truncating-copy"),
        r#"
        (lambda (str size)
            (if (zero? size)
                ""
                (let ((new_str (make-string size)) (chars-to-copy (min size (string-length str))))
                    ($string-copy-onto! str new_str chars-to-copy)
                    new_str)))"#,
    )
    .unwrap();
    ret.push_eval(
        AstSymbol::new("string-copy"),
        "
        (lambda (str)
            ($string-truncating-copy str (string-length str)))",
    )
    .unwrap();
    ret.push_eval(AstSymbol::new("list->string"), r#"
        (lambda (lst)
            (if (null? lst)
                ""
                (let conv-list ((built-string (make-string 1)) (index 0) (lst-head lst))
                    (cond
                        ((null? lst-head)
                            (if (= (string-length built-string) index)
                                built-string
                                ($string-truncating-copy built-string index)))
                        ((= index (string-length built-string))
                            (conv-list ($string-truncating-copy built-string (* 2 index)) index lst-head))
                        (else
                            (string-set! built-string index (car lst-head))
                            (conv-list built-string (+ 1 index) (cdr lst-head)))))))"#).unwrap();
    ret.push_eval(AstSymbol::new("number->string"), r#"
        (lambda (x)
            (if (zero? x)
                (string-copy "0")
                (let to-string ((x x) (chars '()))
                    (if (zero? x)
                        (list->string chars)
                        (let* ((digits "0123456789") (digit (string-ref digits (remainder x 10))) (rest (quotient x 10)))
                            (to-string rest (cons digit chars)))))))"#
    ).unwrap();
    ret.push_eval(
        AstSymbol::new("display"),
        r##"
        (lambda (x)
            (let display ((x x))
                (cond
                    ((string? x)
                        (let print-str ((index 0))
                            (if (= (string-length x) index)
                                (if #f #f)
                                (begin
                                    (write-char (string-ref x index))
                                    (print-str (+ 1 index))))))
                    ((number? x) (display (number->string x)))
                    ((boolean? x) (if x (display "#t") (display "#f")))
                    (else (display "#Unwriteable_object")))))"##,
    )
    .unwrap();
    let newline_str: SchemeString = "\n".parse().unwrap();
    ret.push_object(AstSymbol::new("$newline-str"), newline_str.into());
    ret.push_eval(
        AstSymbol::new("newline"),
        "(lambda () (display $newline-str))",
    )
    .unwrap();

    ret
}

fn gen_main_environment() -> BaseEnvironment {
    gen_scheme_environment()
}

thread_local! {
    pub static SCHEME_ENVIORNMENT: BaseEnvironment = gen_scheme_environment();

    pub static MAIN_ENVIRONMENT: BaseEnvironment = gen_main_environment();
}
