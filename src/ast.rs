//! Datastructures to support an elm AST
//!
//! The AST is built around meaningfull data
//! rather than code representation. Though
//! it should be fine to convert back to code
//! from the AST, with minor loss.
//!
//! # Planned
//! An actual AST instead of just a list of top level declaration

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

#[derive(Debug,Clone)]
pub enum TopDeclr {
    OperPriority(OperPriority),
    DocString(String),
    TypeDeclr(TypeDeclr),
    TypeAlias(TypeAlias),
    FunctionAnnotation(bool, Name, Type),
    OperatorAnnotation(Name, Type),
    FunctionDeclr(Name, Vec<Pattern>, Expression),
    OperatorDeclr(Name, Vec<Pattern>, Expression),
}

#[derive(Debug,Clone)]
pub struct TypeAlias {
    pub name: Name,
    pub type_variables: Vec<Name>,
    pub type_ : Type,
}

#[derive(Debug,Clone)]
pub struct TypeDeclr {
    pub name: Name,
    /// Generic type variables arguments to types
    /// `Either a b`
    pub type_variables: Vec<Name>,
    /// List of alternatives
    pub alternatives: Vec<(Name, Vec<Type>)>,
}

#[derive(Debug,Clone)]
pub struct OperPriority {
    pub associativity: Either<(),()>,
    pub priority: u8,
    pub operator: Operator,
}

#[derive(Debug,Clone)]
pub enum Type {
    /// Terminal is the name of a type.
    Terminal(Name),
    /// A type variable, it is a lowercase type.
    Variable(Name),
    Tuple(Vec<Type>),
    /// A record, can have typed fields, can be an anonymous extensible
    /// record `{ a | fields : Blah }`
    Record(Record),
    /// A function with arity 1 or more (in fact, with curry everything is of
    /// arity 1 or 0, but who cares (hint: not me))
    Function(Vec<Type>),
    /// A type application, such as `Task (List String) (a, b -> c)`
    /// (when the kind of a type is 1 or more, it needs an "argument"
    /// to become an actual meaningfull type)
    Application(Name, Vec<Type>),
    /// The empty record `{}`
    EmptyRecord,
    /// Honestly my favorite thing about fancy type systems
    UnitType,
}

#[derive(Debug,Clone)]
pub struct Record {
    pub variable_over : Option<Name>,
    pub fields : Vec<(Name, Type)>,
}

#[derive(Debug,Clone)]
pub enum Pattern {
    ArgConstructor(Name, Vec<Pattern>),
    Record(Vec<Name>),
    AliasBind(Name, Box<Pattern>),
    Tuple(Vec<Pattern>),
    UnitType,
    Constructor(Name),
    Bind(Name),
    StringLit(String),
    Character(String),
    Number(String),
    Discard,
    EmptyList,
    List(Vec<Pattern>),
    Decons(Vec<Pattern>),
}

#[derive(Debug,Clone)]
pub enum Expression {
    Record(Option<Name>, Vec<(Name, Expression)>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    StringLit(String),
    Character(String),
    Number(String),
    UnitType,
    EmptyRecord,
    EmptyList,
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>),
    LetIn(Vec<Either<LetDeclaration,LetBind>>, Box<Expression>),
    CaseOf(Box<Expression>, Vec<(Pattern, Expression)>),
    Lambda(Vec<Pattern>, Box<Expression>),
    InfixApplication(Vec<(Expression, Operator)>, Box<Expression>),
    Application(Vec<Expression>),
    Variable(Name),
    PrefixOperator(Operator),
    // Argument is arrity of the constructor
    TupleConstructor(i16),
}

#[derive(Debug,Clone)]
pub struct LetDeclaration {
    pub annotation: Option<Type>,
    pub name: Name,
    pub arguments: Vec<Pattern>,
    pub body: Expression,
}

#[derive(Debug,Clone)]
pub struct LetBind {
    pub pattern: Pattern,
    pub body: Expression
}


pub struct LazilyParsed<I,T>(Either<I,Option<T>>)
    where I: IntoIterator<Item=ElmToken>;

impl <I,T>LazilyParsed<I,T>
    where I: IntoIterator<Item=ElmToken>,
          T: Parsable<I> + Sized
{
    fn content(self) -> Option<T> {
        match self.0 {
            Left(unparsed) => T::parse(unparsed), //TODO: keep result of parse
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
    where I: IntoIterator<Item=ElmToken>,
          Self: Sized,
{
    /// parse, with success or failure
    fn parse(I) -> Option<Self>;
}
