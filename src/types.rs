use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SchemePair(Rc<RefCell<SchemeType>>, Rc<RefCell<SchemeType>>);

#[derive(Clone, Debug)]
pub enum SchemeType {
    Pair(SchemePair),
    Number(u64),
    String(String),
    Symbol(String),
    EmptyList,
}
