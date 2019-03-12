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

use getopts::Options;
use std::env;
use std::fs::File;
use std::io::prelude::*;

mod ast;

mod parser;
mod types;

mod interperter;

#[cfg(test)]
mod tests;

fn print_usage(name: &str) {
    println!("Usage: {} PROGRAM", name)
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let self_name = args[0].clone();

    let opts = Options::new();
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(e) => panic!(e.to_string()),
    };

    let file_name = if matches.free.len() == 1 {
        matches.free[1].clone()
    } else if matches.free.is_empty() {
        print_usage(&self_name);
        return;
    } else {
        print_usage(&self_name);
        println!();
        println!("Only one file must be specified");
        return;
    };

    let mut file = File::open(file_name).unwrap();
    let mut prog = String::new();
    file.read_to_string(&mut prog).unwrap();

    println!("{:?}", interperter::eval(&prog).unwrap());
}
