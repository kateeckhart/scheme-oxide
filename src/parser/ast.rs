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
pub enum RuntimeError {
    NotANumber,
    UnknownFunction,
}

impl AstNode {
    pub fn exec(&self) -> Result<SchemeType, RuntimeError> {
        match self {
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
    }
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
