use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use types::*;

#[derive(Clone, Debug)]
pub struct SchemePair(Rc<RefCell<(SchemeType, SchemeType)>>);

impl Display for SchemePair {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "(")?;
        let mut first = true;
        for item_or_err in self.iter() {
            if !first {
                write!(f, " ")?
            } else {
                first = false
            }

            if let Ok(item) = item_or_err {
                item.fmt(f)?
            } else if let Err(PairIterError::Improper((next_to_last, last))) = item_or_err {
                write!(f, "{} . {}", next_to_last, last)?;
                break;
            } else {
                return Err(fmt::Error);
            }
        }
        write!(f, ")")
    }
}

impl SchemePair {
    pub fn new(one: SchemeType, two: SchemeType) -> Self {
        SchemePair(Rc::new(RefCell::new((one, two))))
    }

    pub fn get_car(&self) -> SchemeType {
        self.0.borrow().0.clone()
    }

    pub fn get_cdr(&self) -> SchemeType {
        self.0.borrow().1.clone()
    }

    pub fn set_car(&self, car: SchemeType) {
        let mut self_ref = self.0.borrow_mut();
        self_ref.0 = car;
    }

    pub fn set_cdr(&self, cdr: SchemeType) {
        let mut self_ref = self.0.borrow_mut();
        self_ref.1 = cdr;
    }

    pub fn iter(&self) -> PairIter {
        PairIter {
            pair: Some(self.clone()),
        }
    }
}

pub struct PairIter {
    pair: Option<SchemePair>,
}

#[derive(Debug)]
pub enum PairIterError {
    Circular,
    Improper((SchemeType, SchemeType)),
}

impl Iterator for PairIter {
    type Item = Result<SchemeType, PairIterError>;

    fn next(&mut self) -> Option<Self::Item> {
        let pair = if let Some(ref pair) = self.pair {
            pair.0.clone()
        } else {
            return None;
        };
        let pair_ref = RefCell::borrow(&pair);

        Some(if let SchemeType::Pair(ref next) = pair_ref.1 {
            self.pair = Some(next.clone());
            Ok(pair_ref.0.clone())
        } else if let SchemeType::EmptyList = pair_ref.1 {
            self.pair = None;
            Ok(pair_ref.0.clone())
        } else {
            Err(PairIterError::Improper((
                pair_ref.0.clone(),
                pair_ref.1.clone(),
            )))
        })
    }
}
