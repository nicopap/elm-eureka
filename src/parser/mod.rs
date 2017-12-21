//! An efficient implementation of an elm parser
pub mod tree;
mod grammar;
mod streamed;
mod filter_indent;
#[cfg(test)] mod test;

pub use self::streamed::Parser;
