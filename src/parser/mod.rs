//! An efficient implementation of an elm parser
pub mod tree;
mod grammar;
mod streamed;


/// This needs a lot of work. Planned features:
///
/// * Lazily evaluate the source file rather than collect
///   preemptively all the AST
pub use self::streamed::Parser;
