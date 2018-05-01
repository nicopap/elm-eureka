//! An elm parse tree
//!
//! This is a collection of structures to represent
//! parsed elm files.
//!
//! This is not intended for use by external crate.
//! I intend to only export the types that enter into
//! the construction of the global parse tree (`Module`)
#![allow(non_camel_case_types)]

pub type Anchored<object,attribute> = (attribute, object);

pub type Name = String;
pub type Operator = String;

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
pub struct Import {
    pub global_name: Name,
    pub local_name: Option<Name>,
    pub exposes: Option<ExportList>,
}

// TODO: use explicit records with fields instead of tuples
#[derive(Debug,Clone)]
pub enum TopDeclr<name,annot> {
    OperPriority(OperPriority),
    DocComment(String),
    TypeDeclr(TypeDeclr),
    TypeAlias(TypeAlias),
    FunctionAnnotation(bool, name, Type),
    OperatorAnnotation(name, Type),
    FunctionDeclr(name, Vec<Pattern>, Expression<name,annot>),
    OperatorDeclr(name, Vec<Pattern>, Expression<name,annot>),
    OperatorPrioDeclr {
        function: name,
        name: name,
        priority: OperPriority,
    },
}

#[derive(Debug,Clone)]
pub enum FunctionKind {
    Operator,
    Regular,
}

#[derive(Debug,Clone)]
pub struct Function<name,annot> {
    pub annotation: Option<Type>,
    pub doc: Option<String>,
    pub name: name,
    pub kind: FunctionKind,
    pub arguments: Vec<Pattern>,
    pub body: Expression<name,annot>,
}

#[derive(Debug,Clone)]
pub struct Port<name> {
    pub annotation: Type,
    pub doc: Option<String>,
    pub name: name,
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
pub enum Associativity { Left, Right, Non }

#[derive(Debug,Clone)]
pub struct OperPriority {
    pub associativity: Associativity,
    pub precedence: u8,
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

pub type Expression<name,annot> = Anchored<Expression_<name,annot>,annot>;

#[derive(Debug,Clone)]
pub enum Expression_<name,annot> {
    Record {
        updates: Option<name>,
        fields: Vec<(name, Expression<name,annot>)>,
    },
    List(Vec<Expression<name,annot>>),
    Tuple(Vec<Expression<name,annot>>),
    StringLit(String),
    Character(String),
    Number(String),
    UnitType,
    EmptyRecord,
    EmptyList,
    IfThenElse {
        condition: Box<Expression<name,annot>>,
        then_branch: Box<Expression<name,annot>>,
        else_branch: Box<Expression<name,annot>>,
    },
    LetIn {
        declarations: Vec<LetDeclaration<name,annot>>,
        expression: Box<Expression<name,annot>>
    },
    /// case `condition` of (`branches`)
    CaseOf {
        condition: Box<Expression<name,annot>>,
        branches: Vec<(Pattern, Expression<name,annot>)>,
    },
    Lambda {
        arguments: Vec<Pattern>,
        body: Box<Expression<name,annot>>,
    },
    /// [(`Expression<name,annot>` `op`) (`Expression<name,annot>` `op`) ... `op`)] `trailing`
    InfixApplication {
        prefixes: Vec<(Expression<name,annot>, name)>,
        trailing: Box<Expression<name,annot>>,
    },
    /// A function application the list size is always equal or greater than
    /// 2, includes the function and the various arguments applied to it.
    Application(Vec<Expression<name,annot>>),
    Variable(name),
    /// An operator surounded by `()` to make it prefix. Such as `(+)`
    PrefixOperator(name),
    /// the argument value is the size of the resulting tuple
    TupleConstructor(i16),
}

#[derive(Debug,Clone)]
pub struct LetDeclaration<name,annot> {
    pub annotation: Option<Type>,
    pub name: Option<name>,
    pub arguments: Vec<Pattern>,
    pub body: Expression<name,annot>,
}

#[derive(Debug,Clone)]
pub struct Module<name,annot> {
    pub name: Option<name>,
    pub exports: ExportList,
    pub doc: Option<String>,
    pub imports: Vec<Import>,
    pub types: Vec<TypeDeclaration>,
    pub functions: Vec<Function<name,annot>>,
    pub infixities: Vec<OperPriority>,
    pub ports: Option<Vec<Port<name>>>,
}

pub struct CoalecedTopDeclr<name,annot> {
    pub ports: Vec<Port<name>>,
    pub infixities: Vec<OperPriority>,
    pub functions: Vec<Function<name,annot>>,
    pub types: Vec<TypeDeclaration>,
}

pub fn into_tree<I,name,annot>(declrs : I) -> CoalecedTopDeclr<name,annot>
    where I:Iterator<Item=TopDeclr<name,annot>> + Sized
{
    use self::TopDeclr as TD;

    let mut ports = Vec::new();
    let mut infixities = Vec::new();
    let mut functions = Vec::new();
    let mut types = Vec::new();
    let mut last_doc : Option<String> = None;
    let mut last_annotation : Option<Type> = None;
    for top_declr in declrs { match top_declr {
        TD::OperPriority(oper_priority) => {
            infixities.push(oper_priority);
        },
        TD::DocComment(content) => {
            last_doc = Some(content);
        },
        TD::TypeDeclr(TypeDeclr{name,type_variables,alternatives}) =>{
            let doc = last_doc.take();
            let genre = TypeGenre::Full(alternatives);
            types.push(TypeDeclaration {
                name, type_variables, genre, doc
            })
        },
        TD::TypeAlias(TypeAlias{name, type_, type_variables}) => {
            let doc = last_doc.take();
            let genre = TypeGenre::Alias(type_);
            types.push(TypeDeclaration {
                name, type_variables, genre, doc
            })
        },
        TD::FunctionAnnotation(true, name, annotation) => {
            let doc = last_doc.take();
            ports.push(Port {doc, name, annotation})
        },
        TD::FunctionAnnotation(false, _, type_)
        | TD::OperatorAnnotation(_, type_) => {
            last_annotation = Some(type_);
        },
        TD::FunctionDeclr(name, arguments, body) => {
            let doc = last_doc.take();
            let kind = FunctionKind::Regular;
            let annotation = last_annotation.take();
            functions.push(Function {
                annotation, doc, name, kind, arguments, body,
            })
        },
        TD::OperatorDeclr(name, arguments, body) => {
            let doc = last_doc.take();
            let kind = FunctionKind::Operator;
            let annotation = last_annotation.take();
            functions.push(Function {
                annotation, doc, name, kind, arguments, body,
            })
        },
        TD::OperatorPrioDeclr { .. } => {
            unimplemented!() //TODO: elm 0.19 feature
        },
    } }
    // TODO: add documentation to infix operators created with the `infix`
    // syntax (copy the one from the corresponding function).
    CoalecedTopDeclr {ports, infixities, functions, types}
}

