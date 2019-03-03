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

use crate::types::pair::ListFactory;
use crate::types::*;
use std::cell::Cell;
use std::iter::FromIterator;
use std::thread::{self, ThreadId};

thread_local! {
    static TEMP_COUNT: Cell<usize> = Cell::new(0);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreSymbol {
    And,
    Or,
    Let,
    Lambda,
    If,
    GenUnspecified,
}

impl CoreSymbol {
    pub fn get_name(self) -> &'static str {
        match self {
            CoreSymbol::And => "and",
            CoreSymbol::Or => "or",
            CoreSymbol::Let => "let",
            CoreSymbol::Lambda => "lambda",
            CoreSymbol::If => "if",
            CoreSymbol::GenUnspecified => "$gen_unspecified",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum AstSymbolInner {
    Core(CoreSymbol),
    Temp((usize, ThreadId)),
    Defined(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AstSymbol(AstSymbolInner);

impl AstSymbol {
    pub fn new(name: &str) -> AstSymbol {
        AstSymbol(AstSymbolInner::Defined(name.to_string()))
    }

    pub fn gen_temp() -> AstSymbol {
        let count = TEMP_COUNT.with(|count| {
            let ret = count.get();
            count.set(ret + 1);
            if ret == usize::max_value() {
                panic!("Temporary count overflowed!")
            }
            ret
        });

        AstSymbol(AstSymbolInner::Temp((count, thread::current().id())))
    }

    pub fn get_name(&self) -> String {
        match &self.0 {
            AstSymbolInner::Core(core) => core.get_name().to_string(),
            AstSymbolInner::Temp((local_id, _)) => format!("$temp$local_id{}", local_id),
            AstSymbolInner::Defined(name) => name.clone(),
        }
    }
}

impl From<CoreSymbol> for AstSymbol {
    fn from(core: CoreSymbol) -> AstSymbol {
        AstSymbol(AstSymbolInner::Core(core))
    }
}

#[derive(Clone, Debug)]
pub enum ListTerminator {
    EmptyList,
    Node(Box<AstNode>),
}

impl ListTerminator {
    fn is_empty_list(&self) -> bool {
        if let ListTerminator::EmptyList = self {
            true
        } else {
            false
        }
    }

    fn to_datum(&self) -> SchemeType {
        match self {
            ListTerminator::EmptyList => SchemeType::EmptyList,
            ListTerminator::Node(node) => node.to_datum(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AstList {
    nodes: Vec<AstNode>,
    terminator: ListTerminator,
}

impl AstList {
    pub fn none() -> AstList {
        AstList {
            nodes: Vec::new(),
            terminator: ListTerminator::EmptyList,
        }
    }

    pub fn one(node: AstNode) -> AstList {
        AstList {
            nodes: vec![node],
            terminator: ListTerminator::EmptyList,
        }
    }

    pub fn is_empty_list(&self) -> bool {
        self.nodes.is_empty() && self.terminator.is_empty_list()
    }

    pub fn is_improper_list(&self) -> bool {
        !self.terminator.is_empty_list()
    }

    pub fn get_terminator(&self) -> ListTerminator {
        self.terminator.clone()
    }

    pub fn iter(&self) -> impl Iterator<Item = &AstNode> + '_ {
        self.nodes.iter()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}

impl FromIterator<AstNode> for AstList {
    fn from_iter<T>(iter: T) -> AstList
    where
        T: IntoIterator<Item = AstNode>,
    {
        let mut builder = AstListBuilder::new();

        for node in iter {
            builder.push(node)
        }

        builder.build()
    }
}

impl<'a> FromIterator<&'a AstNode> for AstList {
    fn from_iter<T>(iter: T) -> AstList
    where
        T: IntoIterator<Item = &'a AstNode>,
    {
        iter.into_iter().cloned().collect()
    }
}

pub struct AstListBuilder {
    nodes: Vec<AstNode>,
}

impl AstListBuilder {
    pub fn new() -> AstListBuilder {
        AstListBuilder { nodes: Vec::new() }
    }

    pub fn push(&mut self, node: AstNode) {
        self.nodes.push(node)
    }

    fn build_with_terminator(mut self, terminator: ListTerminator) -> AstList {
        self.nodes.shrink_to_fit();
        AstList {
            nodes: self.nodes,
            terminator,
        }
    }

    pub fn build(self) -> AstList {
        self.build_with_terminator(ListTerminator::EmptyList)
    }

    pub fn build_with_tail(mut self, node: AstNode) -> AstList {
        if let AstNodeInner::List(mut list) = node.0 {
            if self.nodes.is_empty() {
                self.build()
            } else {
                self.nodes.append(&mut list.nodes);
                self.build_with_terminator(list.terminator)
            }
        } else {
            self.build_with_terminator(ListTerminator::Node(Box::new(node)))
        }
    }
}

#[derive(Clone, Debug)]
enum AstNodeInner {
    Number(i64),
    Symbol(AstSymbol),
    String(String),
    List(AstList),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub struct AstNode(AstNodeInner);

impl AstNode {
    pub fn from_number(number: i64) -> AstNode {
        AstNode(AstNodeInner::Number(number))
    }

    pub fn from_string(string: String) -> AstNode {
        AstNode(AstNodeInner::String(string))
    }

    pub fn from_bool(boolean: bool) -> AstNode {
        AstNode(AstNodeInner::Bool(boolean))
    }

    pub fn to_datum(&self) -> SchemeType {
        match &self.0 {
            AstNodeInner::Number(x) => SchemeType::Number(*x),
            AstNodeInner::Symbol(sym) => SchemeType::Symbol(sym.get_name()),
            AstNodeInner::String(stri) => SchemeType::String(stri.clone()),
            AstNodeInner::List(list) => {
                let mut builder = ListFactory::new();

                for node in list.iter() {
                    builder.push(node.to_datum())
                }

                builder.build_with_tail(list.terminator.to_datum()).into()
            }
            AstNodeInner::Bool(boolean) => SchemeType::Bool(*boolean),
        }
    }

    pub fn to_symbol(&self) -> Option<AstSymbol> {
        if let AstNodeInner::Symbol(sym) = &self.0 {
            Some(sym.clone())
        } else {
            None
        }
    }

    pub fn to_list(&self) -> Option<AstList> {
        if let AstNodeInner::List(list) = &self.0 {
            Some(list.clone())
        } else {
            None
        }
    }
}

impl From<CoreSymbol> for AstNode {
    fn from(sym: CoreSymbol) -> AstNode {
        let ast_sym: AstSymbol = sym.into();
        ast_sym.into()
    }
}

impl From<AstSymbol> for AstNode {
    fn from(sym: AstSymbol) -> AstNode {
        AstNode(AstNodeInner::Symbol(sym))
    }
}

impl From<AstList> for AstNode {
    fn from(list: AstList) -> AstNode {
        AstNode(AstNodeInner::List(list))
    }
}
