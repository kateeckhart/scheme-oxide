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

use parser::ast::AstNode;
use types::*;

#[derive(Debug)]
pub enum RuntimeError {
    NotANumber,
    UnknownFunction,
}

/*pub fn exec_ast(ast: &AstNode) -> Result<SchemeType, RuntimeError> {
    match ast {
        AstNode::Literal(datum) => Ok(datum.clone()),
        AstNode::Call { name, params } => {
            if name.name == "+" {
                let mut res = 0;
                for x in params {
                    if let SchemeType::Number(num) = x.exec()? {
                        res += num
                    } else {
                        return Err(RuntimeError::NotANumber);
                    }
                }

                Ok(SchemeType::Number(res))
            } else {
                return Err(RuntimeError::UnknownFunction);
            }
        }
        _ => unimplemented!(),
    }
}*/

