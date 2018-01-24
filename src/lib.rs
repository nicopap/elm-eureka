// Copyright 2018 Nicola Papale. This program is distributed under the terms of
// the GNU Lesser GPL.
//
// elm-eureka is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// elm-eureka is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with elm-eureka.  If not, see <http://www.gnu.org/licenses/>.
//! Elm eureka is a rust library for parsing elm projects and elm code.
//!
//! ## Goal
//!
//! The aim is to facilitate the creation of efficient tooling for the [elm
//! programming language](http://elm-lang.org).
//!
//!
//! ## Current features
//!
//! * A lexer capable of tokenizing all elm constructs from a character iterator.
//!
//! * A parser capable of parsing the entirety of the elm grammar, at the exception
//!   of a couple edge cases evocated in the [limitations](#limitations) section.
//!
//!
//! ## Implementation
//!
//! The architecture is based on the usual lexer+parser passes. Since the goal is
//! to have an efficient parser, I made it *lazy*. Lazy in the sense that it will
//! only attempt to parse expressions *needed* to get a specific information on
//! some code.
//!
//! The typical usecase for the parser is to get the list of exported symbols in a
//! file, this usually requires to read the `module` declaration and nothing else!
//!
//! If you want to visualise the dependency graph of your application, it is also
//! only a matter of the first few lines of a source file!
//!
//! Finally, if you want to retreive the type of the exported values, you need only
//! to look at a very limited subset of the file. In fact, you don't even need to
//! parse any expression.
//!
//!
//! ## Planned (maybe?)
//!
//! * An efficient parser capable of building an elm AST semi-lazily. (Currently,
//!   it parses everything if anything else than the imports, exports and module
//!   docs are needed)
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
//!
//! * Proper error handling :)
//!
//! * Proper handling of shader and multiline string literals.
//!
//! ## Limitations
//!
//! * This is not the official elm compiler and the parser is homebrew rather
//!   than taked from source (Rust and Haskell are two different languages with
//!   very different limitations that leads to verify different implementations)
//!
//! * This means that there will be some edge cases where `elm-make` accepts
//!   a program and elm-eureka do not (and vis-versa). An example of such case is
//!   aligned expressions such as `case` and `let`. elm-eureka will typically not
//!   accept aligned branches if the first one is on the same line as the `let` or
//!   `case` keyword.
//!
//! example:
//! ```elm
//! -- Does not work (should work)
//! f x = let y = x * 3
//!           z = x + 92
//!       in x + y + z
//! -- Works (should)
//! f x =
//!   let
//!       y = x * 3
//!       z = x + 92
//! 	in
//!       x + y + z
//! -- Works (should)
//! f x = let y = 10 in y * x
//!
//! -- Works (should)
//! f x =
//!   let x = 10
//!       y = 20
//!   in x + y
//! ```
//!
//! * As you can see, I try my best at accepting *some* of the edge cases.
//!
//! * Prefix `-`. The weird semantic makes it hard to integrate in a lexer:
//!   * If preceeding without whitespace a word or opening parenthesis, and there
//!     is no words preceeding the `-` without whitespace, it is a prefix `-`.
//!   * Otherwise it is the `-` operator.
//!   * The current parser's grammar do not handle prefix `-`, instead I do
//!     some hardcoding at the lexing phase. Proper handling of prefix `-` is
//!     planned.
//!
//! * Shader literals and multiline string literals are not yet handled properly.
//!
//! ## Usage
//!
//! Please see the "examples" directory for usage examples.
//!
//! ## Stability
//!
//! I'm still working on the API. I have API change plans that I still didn't put
//! to execution, so be aware.
//!
//! ## License(s)
//!
//! * The libraries are licensied under the LGPL3. Please see
//!   <https://www.gnu.org/licenses/lgpl-3.0.html> for more informations.
//!
//! * I do not claim any copyrights over the files in "examples" directories. They
//! 	are trivial generic implementations with no claims of fitness for any
//! 	purpose.
extern crate serde_json;
extern crate itertools;
extern crate walkdir;
#[cfg(test)] #[macro_use] extern crate pretty_assertions;

pub mod packages_reader;
mod tokens;
mod position;
pub mod lexer;
pub mod parser;
pub use tokens::ElmToken;
pub use parser::Parser;
