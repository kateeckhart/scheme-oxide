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

macro_rules! bind_scheme {
    (@raw pub $name:ident = $gen:expr) => {
        pub fn $name() -> $crate::types::SchemeType {
            thread_local! {
                static SINGLETON: $crate::types::SchemeType = $gen;
            }
            SINGLETON.with(::std::clone::Clone::clone)
        }
    };
    (pub $name:ident @unique) => {
        bind_scheme!(@raw pub $name = $crate::types::SchemeObject::unique_new().into());
    };
    (pub $name:ident = $scheme:expr) => {
        bind_scheme!(@raw pub $name =
            $crate::interpreter::runtime_environment::STAGE1_ENVIRONMENT.with(|env| env.eval_str($scheme)).unwrap());
    };
    (pub fn $name:ident($( $arg_name:ident ),*) = $scheme:expr) => {
        pub fn $name($($arg_name: $crate::types::SchemeType),*) -> ::std::result::Result<$crate::types::SchemeType, $crate::interpreter::RuntimeError> {
            bind_scheme!(pub object = $scheme);
            let fun = object().to_function().unwrap();
            fun.call(vec![$($arg_name),*])
        }
    };
}

bind_scheme!(pub s_true @unique);
bind_scheme!(pub s_false @unique);

bind_scheme!(pub empty_list = "$empty-list");
//bind_scheme!(pub immutable_pair_type_id = "$immutable-pair-type-id");
//bind_scheme!(pub mutable_pair_type_id = "$mutable-pair-type-id");
bind_scheme!(pub symbol_type_id = "$symbol-type-id");

bind_scheme!(pub fn car(list) = "car");
bind_scheme!(pub fn cdr(list) = "cdr");
bind_scheme!(pub fn make_list_factory(is_mutable) = "$make-list-factory");
