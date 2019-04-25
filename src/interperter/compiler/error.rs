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

use crate::ast::{AstNode, AstSymbol};

#[derive(Debug)]
enum CompilerErrorType {
    ArgError,
    IdentifierNotFound,
    SyntaxError,
    WrongType,
}

#[derive(Debug)]
pub struct CompilerError {
    e_type: CompilerErrorType,
    message: String,
}

impl CompilerError {
    pub fn argc(what: &str, expected: &str, actual: usize) -> Self {
        Self {
            e_type: CompilerErrorType::ArgError,
            message: format!("{} expected {} arguments got {}.", what, expected, actual),
        }
    }

    pub fn syntax(msg: &str) -> Self {
        Self {
            e_type: CompilerErrorType::SyntaxError,
            message: msg.to_string(),
        }
    }

    pub fn identifier_not_found(ident: &str) -> Self {
        Self {
            e_type: CompilerErrorType::IdentifierNotFound,
            message: format!("{} is not defined.", ident),
        }
    }

    fn wrong_type(what: &str, expected: &str, got: &str) -> Self {
        Self {
            e_type: CompilerErrorType::WrongType,
            message: format!("{} needs a {} got a {} instead.", what, expected, got),
        }
    }
}

pub trait AstCastErrorImpl {
    type CastExpected;

    fn into_compiler_result(self, what: &str) -> Result<Self::CastExpected, CompilerError>;
}

impl AstCastErrorImpl for Result<AstSymbol, AstNode> {
    type CastExpected = AstSymbol;

    fn into_compiler_result(self, what: &str) -> Result<AstSymbol, CompilerError> {
        self.map_err(|err| CompilerError::wrong_type(what, "symbol", err.get_name()))
    }
}

impl AstCastErrorImpl for Result<Vec<AstNode>, AstNode> {
    type CastExpected = Vec<AstNode>;

    fn into_compiler_result(self, what: &str) -> Result<Vec<AstNode>, CompilerError> {
        self.map_err(|err| CompilerError::wrong_type(what, "proper list", err.get_name()))
    }
}
