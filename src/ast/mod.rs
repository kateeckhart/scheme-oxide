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
use std::thread::{self, ThreadId};

thread_local! {
    static TEMP_COUNT: Cell<u64> = Cell::new(0);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreSymbol {
    And,
    Begin,
    Or,
    Let,
    Lambda,
    If,
    Set,
    GenUnspecified,
}

impl CoreSymbol {
    pub fn get_name(self) -> &'static str {
        match self {
            CoreSymbol::And => "and",
            CoreSymbol::Begin => "begin",
            CoreSymbol::Or => "or",
            CoreSymbol::Let => "let",
            CoreSymbol::Lambda => "lambda",
            CoreSymbol::If => "if",
            CoreSymbol::Set => "set",
            CoreSymbol::GenUnspecified => "$gen_unspecified",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum AstSymbolInner {
    Core(CoreSymbol),
    Temp((u64, ThreadId)),
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
            if ret == u64::max_value() {
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

#[derive(Clone, Debug, PartialEq)]
enum ListType {
    Proper,
    Improper(Box<AstNode>),
}

impl ListType {
    fn is_improper_list(&self) -> bool {
        if let ListType::Improper(_) = self {
            true
        } else {
            false
        }
    }

    fn to_datum(&self) -> SchemeType {
        match self {
            ListType::Proper => SchemeType::EmptyList,
            ListType::Improper(node) => node.to_datum(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstList {
    nodes: Vec<AstNode>,
    list_type: ListType,
}

impl AstList {
    pub fn none() -> AstList {
        AstList {
            nodes: Vec::new(),
            list_type: ListType::Proper,
        }
    }

    pub fn one(node: AstNode) -> AstList {
        AstList {
            nodes: vec![node],
            list_type: ListType::Proper,
        }
    }

    pub fn is_improper_list(&self) -> bool {
        self.list_type.is_improper_list()
    }

    pub fn as_proper_list(&self) -> Option<&[AstNode]> {
        if let ListType::Proper = self.list_type {
            Some(&self.nodes)
        } else {
            None
        }
    }

    pub fn into_proper_list(self) -> Result<Vec<AstNode>, AstList> {
        if let ListType::Proper = self.list_type {
            Ok(self.nodes)
        } else {
            Err(self)
        }
    }

    pub fn into_improper_list(self) -> Result<ImproperList, AstList> {
        if let ListType::Improper(terminator) = self.list_type {
            Ok(ImproperList {
                nodes: self.nodes,
                terminator: *terminator,
            })
        } else {
            Err(self)
        }
    }
}

impl From<Vec<AstNode>> for AstList {
    fn from(list: Vec<AstNode>) -> AstList {
        AstList {
            nodes: list,
            list_type: ListType::Proper,
        }
    }
}

pub struct ImproperList {
    pub nodes: Vec<AstNode>,
    pub terminator: AstNode,
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

    fn build_with_type(mut self, list_type: ListType) -> AstList {
        self.nodes.shrink_to_fit();
        AstList {
            nodes: self.nodes,
            list_type,
        }
    }

    pub fn build(self) -> AstList {
        self.build_with_type(ListType::Proper)
    }

    pub fn build_with_tail(mut self, node: AstNode) -> Option<AstList> {
        if let AstNodeInner::List(mut list) = node.0 {
            if self.nodes.is_empty() && list.is_improper_list() {
                return None;
            }

            self.nodes.append(&mut list.nodes);

            Some(self.build_with_type(list.list_type))
        } else {
            Some(self.build_with_type(ListType::Improper(Box::new(node))))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum AstNodeInner {
    Number(i64),
    Symbol(AstSymbol),
    String(String),
    List(AstList),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
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

                for node in list.nodes.iter() {
                    builder.push(node.to_datum())
                }

                builder.build_with_tail(list.list_type.to_datum()).into()
            }
            AstNodeInner::Bool(boolean) => SchemeType::Bool(*boolean),
        }
    }

    pub fn as_list(&self) -> Option<&AstList> {
        if let AstNodeInner::List(list) = &self.0 {
            Some(list)
        } else {
            None
        }
    }

    pub fn as_proper_list(&self) -> Option<&[AstNode]> {
        if let AstNodeInner::List(list) = &self.0 {
            list.as_proper_list()
        } else {
            None
        }
    }

    pub fn into_symbol(self) -> Result<AstSymbol, AstNode> {
        if let AstNodeInner::Symbol(sym) = self.0 {
            Ok(sym)
        } else {
            Err(self)
        }
    }

    pub fn into_list(self) -> Result<AstList, AstNode> {
        if let AstNodeInner::List(list) = self.0 {
            Ok(list)
        } else {
            Err(self)
        }
    }

    pub fn into_proper_list(self) -> Result<Vec<AstNode>, AstNode> {
        let list = self.into_list()?;

        list.into_proper_list()
            .map_err(|list| AstNode(AstNodeInner::List(list)))
    }

    pub fn to_symbol(&self) -> Option<AstSymbol> {
        if let AstNodeInner::Symbol(sym) = &self.0 {
            Some(sym.clone())
        } else {
            None
        }
    }

    pub fn is_improper_list(&self) -> bool {
        if let Some(list) = self.as_list() {
            list.is_improper_list()
        } else {
            false
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

impl From<Vec<AstNode>> for AstNode {
    fn from(list: Vec<AstNode>) -> AstNode {
        let list_object: AstList = list.into();
        list_object.into()
    }
}
