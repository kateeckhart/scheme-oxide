/*
    Copyright 2018 Alexander Eckhart

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

use types::pair::PairIterError;
use types::*;

#[derive(Debug)]
pub struct Identifier {
    name: String,
}

#[derive(Debug)]
pub enum AstNode {
    Literal(SchemeType),
    Call {
        name: Identifier,
        params: Vec<AstNode>,
    },
    Var(Identifier),
}


#[derive(Debug)]
pub enum AstError {
    SyntaxError,
    InvalidList(PairIterError),
}

impl From<PairIterError> for AstError {
    fn from(err: PairIterError) -> AstError {
        AstError::InvalidList(err)
    }
}

pub fn gen_ast(datums: SchemePair) -> Result<Vec<AstNode>, AstError> {
    let mut ast = Vec::new();
    for datum in datums.iter() {
        ast.push(match datum? {
            SchemeType::String(string) => AstNode::Literal(SchemeType::String(string)),
            SchemeType::Number(num) => AstNode::Literal(SchemeType::Number(num)),
            SchemeType::Symbol(name) => AstNode::Var(Identifier { name }),
            SchemeType::Pair(call) => {
                let name = if let SchemeType::Symbol(n) = call.get_car() {
                    Identifier { name: n }
                } else {
                    return Err(AstError::SyntaxError);
                };

                let params = if let SchemeType::Pair(p) = call.get_cdr() {
                    p
                } else {
                    return Err(AstError::SyntaxError);
                };

                AstNode::Call {
                    name,
                    params: gen_ast(params)?,
                }
            }
            _ => return Err(AstError::SyntaxError),
        })
    }
    Ok(ast)
}
