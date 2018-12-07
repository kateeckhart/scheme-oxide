use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SchemePair(Rc<RefCell<(SchemeType, SchemeType)>>);

impl SchemePair {
    pub fn new(one: SchemeType, two: SchemeType) -> Self {
        SchemePair(Rc::new(RefCell::new((one, two))))
    }

    pub fn set_car(&self, car: SchemeType) {
        let mut self_ref = self.0.borrow_mut();
        self_ref.0 = car;
    }

    pub fn set_cdr(&self, cdr: SchemeType) {
        let mut self_ref = self.0.borrow_mut();
        self_ref.1 = cdr;
    }
}

#[derive(Clone, Debug)]
pub enum SchemeType {
    Pair(SchemePair),
    Number(u64),
    String(String),
    Symbol(String),
    EmptyList,
}
