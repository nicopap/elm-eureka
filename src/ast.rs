//! Datastructures to support an elm AST
//!
//! The AST is built around meaningfull data
//! rather than code representation. Though
//! it should be fine to convert back to code
//! from the AST, with minor loss.

use either::Either;
use either::Either::{Left,Right};
use tokens::ElmToken;

pub type Name=String;
pub type Operator=String;

#[derive(Debug,Clone)]
pub enum ExportList {
    Unqualified,
    List(Vec<ExportEntry>),
}

#[derive(Debug,Clone)]
pub enum ExportEntry {
    Name(Name),
    Operator(Operator),
    WithConstructors(Name, Vec<Name>),
    WithAllConstructors(Name),
}

#[derive(Debug,Clone)]
pub struct ModuleDeclr {
    pub name: Name,
    pub exports: ExportList,
}

#[derive(Debug,Clone)]
pub struct ElmImport {
    pub global_name: Name,
    pub local_name: Option<Name>,
    pub exposes: Option<ExportList>,
}

pub struct LazilyParsed<I,T>(Either<I,Option<T>>)
    where I: Iterator<Item=ElmToken>;

impl <I,T>LazilyParsed<I,T>
    where I: Iterator<Item=ElmToken>,
          T: Parsable<I> + Sized
{
    fn content(self) -> Option<T> {
        match self.0 {
            Left(unparsed) => T::parse(unparsed),
            Right(parsed) => parsed,
        }
    }
    fn evaluate(self) -> LazilyParsed<I,T> {
        match self.0 {
            Left(unparsed) => LazilyParsed(Right(T::parse(unparsed))),
            Right(_) => self,
        }
    }
}

/// An entity (usually a node of the AST) that can be parsed
/// from an ordered collection of tokens.
pub trait Parsable<I>
    where I: Iterator<Item=ElmToken>,
          Self: Sized,
{
    /// parse, with success or failure
    fn parse(I) -> Option<Self>;
}
