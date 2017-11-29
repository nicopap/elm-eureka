//! An efficient implementation of an elm parser
pub mod tree;
mod grammar;
mod streamed;

pub use self::streamed::Parser;
