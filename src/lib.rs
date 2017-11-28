//! Elm eureka is a rust library for parsing elm projects and elm code.
//!
//! ## Goal
//!
//! The aim is to facilitate the creation of efficient tooling for the [elm
//! programming language](http://elm-lang.org).
//!
//!
//! ## Current state
//!
//! I've just done the lexer. Even then, the lexer is not that accurate, it doesn't
//! really vaidate literals.
//!
//!
//! ## Planned (maybe?)
//!
//! * An efficient parser capable of building an elm AST.
//!
//! * Proper line location tracking, so you can retreive the location of the
//!   definition of some functions.
//!
//! * A clean API to just retreive interesting stuff from the code (types, doc
//!   strings, exported symbols etc)
//!
//! * Other crates would implement the actual user-facing part of dev tools. One
//!   can imagine a completion manager with caching, or a dependency graph
//!   generator etc.
extern crate serde_json;
extern crate itertools;
extern crate either;

pub mod packages_reader;
mod tokens;
mod lexer;
mod parser;
pub use tokens::ElmToken;
pub use lexer::LexableIterator;
pub use parser::Parser;
pub use parser::tree;
