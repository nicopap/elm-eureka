//! An elm parse tree
//!
//! This is a collection of structures to represent
//! parsed elm files.
//!
//! This is not intended for use by external crate.
//! I intend to only export the types that enter into
//! the construction of the global parse tree (ElmModule)


use either::Either;

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
pub enum FunctionKind {
    Operator,
    Regular,
}

#[derive(Debug,Clone)]
pub struct FunctionDeclaration {
    pub annotation: Option<Type>,
    pub doc: Option<String>,
    pub name: Name,
    pub kind: FunctionKind,
    pub arguments: Vec<Pattern>,
    pub body: Expression
}

#[derive(Debug,Clone)]
pub struct PortDeclaration {
    pub annotation: Type,
    pub doc: Option<String>,
    pub name: Name,
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
pub enum TypeGenre {
    Alias(Type),
    Full(Vec<(Name,Vec<Type>)>),
}

#[derive(Debug,Clone)]
pub struct TypeDeclaration {
    pub name: Name,
    pub type_variables: Vec<Name>,
    pub genre: TypeGenre,
    pub doc: Option<String>,
}

#[derive(Debug,Clone)]
pub enum Associativity { Left, Right }

#[derive(Debug,Clone)]
pub struct OperPriority {
    pub associativity: Associativity,
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
    // i16 is the arrity of the constructor
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

#[derive(Debug,Clone)]
pub struct ElmModule {
    pub name: Name,
    pub exports: ExportList,
    pub doc: Option<String>,
    pub imports: Vec<ElmImport>,
    pub types: Vec<TypeDeclaration>,
    pub functions: Vec<FunctionDeclaration>,
    pub infixities: Vec<OperPriority>,
    pub ports: Option<Vec<PortDeclaration>>,
}

pub fn into_tree<I:Iterator<Item=TopDeclr> + Sized>(declrs : I)
    -> (Vec<PortDeclaration>,
        Vec<OperPriority>,
        Vec<FunctionDeclaration>,
        Vec<TypeDeclaration>)
{
    use self::TopDeclr as TD;

    let mut ports = Vec::new();
    let mut infixities = Vec::new();
    let mut functions = Vec::new();
    let mut types = Vec::new();
    let mut last_docstring : Option<String> = None;
    let mut last_annotation : Option<Type> = None;
    for top_declr in declrs { match top_declr {
        TD::OperPriority(oper_priority) => {
            infixities.push(oper_priority);
        },
        TD::DocString(content) => {
            last_docstring = Some(content);
        },
        TD::TypeDeclr(TypeDeclr{name, type_variables, alternatives}) => {
            let doc = last_docstring.take();
            let genre = TypeGenre::Full(alternatives);
            types.push(TypeDeclaration {name, type_variables, genre, doc})
        },
        TD::TypeAlias(TypeAlias{name, type_, type_variables}) => {
            let doc = last_docstring.take();
            let genre = TypeGenre::Alias(type_);
            types.push(TypeDeclaration {name, type_variables, genre, doc})
        },
        TD::FunctionAnnotation(true, name, annotation) => {
            let doc = last_docstring.take();
            ports.push(PortDeclaration {doc, name, annotation})
        },
        TD::FunctionAnnotation(false, _, type_) => {
            last_annotation = Some(type_);
        },
        TD::OperatorAnnotation(_, type_) => {
            last_annotation = Some(type_);
        },
        TD::FunctionDeclr(name, arguments, body) => {
            let doc = last_docstring.take();
            let kind = FunctionKind::Regular;
            let annotation = last_annotation.take();
            functions.push(FunctionDeclaration {
                annotation,
                doc,
                name,
                kind,
                arguments,
                body
            })
        },
        TD::OperatorDeclr(name, arguments, body) => {
            let doc = last_docstring.take();
            let kind = FunctionKind::Operator;
            let annotation = last_annotation.take();
            functions.push(FunctionDeclaration {
                annotation,
                doc,
                name,
                kind,
                arguments,
                body
            })
        },
    } }
    (ports, infixities, functions, types)
}

