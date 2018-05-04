//! An elm parse tree
//!
//! This is a collection of structures to represent
//! parsed elm files.
//!
//! This is not intended for use by external crate.
//! I intend to only export the types that enter into
//! the construction of the global parse tree (`Module`)
#![allow(non_camel_case_types)]
use tokens;

pub type Anchored<object,attribute> = (attribute, object);

#[derive(Debug,Clone)]
pub enum ExportList<name,annot> {
    Unqualified,
    List(Vec<ExportEntry<name,annot>>),
}

pub type ExportEntry<name,annot> = Anchored<ExportEntry_<name>,annot>;

#[derive(Debug,Clone)]
pub enum ExportEntry_<name> {
    Name(name),
    Operator(name),
    WithConstructors(name, Vec<name>),
    WithAllConstructors(name),
}

#[derive(Debug,Clone)]
pub struct ModuleDeclr<name,annot> {
    pub name: name,
    pub exports: ExportList<name,annot>,
}

#[derive(Debug,Clone)]
pub struct Import<name,annot> {
    pub global_name: name,
    pub local_name: Option<name>,
    pub exposes: Option<ExportList<name,annot>>,
    pub annot: annot,
}

#[derive(Debug,Clone)]
pub enum TopDeclr<name,annot> {
    OperPriority(OperPriority<name>),
    DocComment(String),
    Type(TypeGenre<name>),
    FunctionAnnotation {
        is_port: bool,
        name: name,
        annotation: Type<name>,
    },
    OperatorAnnotation(name, Type<name>),
    FunctionDeclr {
        name: name,
        arguments: Vec<Pattern<name,annot>>,
        body: Expression<name,annot>,
    },
    OperatorDeclr {
        name: name,
        arguments: Vec<Pattern<name,annot>>,
        body: Expression<name,annot>,
    },
    OperatorPrioDeclr {
        function: name,
        name: name,
        priority: OperPriority<name>,
    },
}

#[derive(Debug,Clone)]
pub enum FunctionKind {
    Operator,
    Regular,
}

#[derive(Debug,Clone)]
pub struct Function<name,annot> {
    pub annotation: Option<Type<name>>,
    pub doc: Option<String>,
    pub name: name,
    pub kind: FunctionKind,
    pub arguments: Vec<Pattern<name,annot>>,
    pub body: Expression<name,annot>,
}

#[derive(Debug,Clone)]
pub struct Port<name> {
    pub annotation: Type<name>,
    pub doc: Option<String>,
    pub name: name,
}

pub type Type<name> = Anchored<Type_<name>,tokens::Location>;
#[derive(Debug,Clone)]
pub enum Type_<name> {
    /// Terminal is the name of a type.
    Terminal(name),
    /// A type variable, it is a lowercase type.
    Variable(name),
    Tuple(Vec<Type<name>>),
    /// A record, can have typed fields, can be an anonymous extensible
    /// record `{ a | fields : Blah }`
    Record {
        variable_over: Option<name>,
        fields: Vec<(name, Type<name>)>,
    },
    /// A function with arity 1 or more (in fact, with curry everything is of
    /// arity 1 or 0, but who cares (hint: not me))
    Function(Vec<Type<name>>),
    /// A type application, such as `Task (List String) (a, b -> c)`
    /// (when the kind of a type is 1 or more, it needs an "argument"
    /// to become an actual meaningfull type)
    Application(name, Vec<Type<name>>),
    /// The empty record `{}`
    EmptyRecord,
    UnitType,
}

pub type TypeGenre<name> = Anchored<TypeGenre_<name>,tokens::Location>;
#[derive(Debug,Clone)]
pub enum TypeGenre_<name> {
    Alias {
        name: name,
        type_variables: Vec<name>,
        type_ : Type<name>,
    },
    Full {
        name: name,
        /// Generic type variables arguments to types
        /// `Either a b`
        type_variables: Vec<name>,
        /// List of alternatives
        alternatives: Vec<Anchored<(name, Vec<Type<name>>),tokens::Location>>,
    },
}

#[derive(Debug,Clone)]
pub enum Associativity { Left, Right, Non }

#[derive(Debug,Clone)]
pub struct OperPriority<name> {
    pub associativity: Associativity,
    pub precedence: u8,
    pub operator: name,
    pub location: tokens::Location,
}


pub type Pattern<name,annot> = Anchored<Pattern_<name,annot>,annot>;

#[derive(Debug,Clone)]
pub enum Pattern_<name,annot> {
    ArgConstructor(name, Vec<Pattern<name,annot>>),
    Record(Vec<name>),
    AliasBind(name, Box<Pattern<name,annot>>),
    Tuple(Vec<Pattern<name,annot>>),
    UnitType,
    Constructor(name),
    Bind(name),
    StringLit(String),
    Character(String),
    Number(String),
    Discard,
    EmptyList,
    List(Vec<Pattern<name,annot>>),
    Decons(Vec<Pattern<name,annot>>),
}

#[derive(Debug,Clone)]
pub enum Literal {
    Char,
    StringL,
    Number,
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
    Literal(Literal, String),
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
        branches: Vec<(Pattern<name,annot>, Expression<name,annot>)>,
    },
    Lambda {
        arguments: Vec<Pattern<name,annot>>,
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
    pub annotation: Option<Type<name>>,
    pub name: Option<name>,
    pub arguments: Vec<Pattern<name,annot>>,
    pub body: Expression<name,annot>,
}

#[derive(Debug,Clone)]
pub struct Module<name,annot> {
    pub name: Option<name>,
    pub exports: ExportList<name,annot>,
    pub doc: Option<String>,
    pub imports: Vec<Import<name,annot>>,
    pub types: Vec<(Option<String>, TypeGenre<name>)>,
    pub functions: Vec<Function<name,annot>>,
    pub infixities: Vec<OperPriority<name>>,
    pub ports: Option<Vec<Port<name>>>,
}

pub struct CoalecedTopDeclr<name,annot> {
    pub ports: Vec<Port<name>>,
    pub infixities: Vec<OperPriority<name>>,
    pub functions: Vec<Function<name,annot>>,
    pub types: Vec<(Option<String>, TypeGenre<name>)>,
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
    let mut last_annotation : Option<Type<name>> = None;
    for top_declr in declrs { match top_declr {
        TD::OperPriority(oper_priority) => {
            infixities.push(oper_priority);
        },
        TD::DocComment(content) => {
            last_doc = Some(content);
        },
        TD::Type(genre) =>{
            let doc = last_doc.take();
            types.push((doc,genre))
        },
        TD::FunctionAnnotation{is_port:true, name, annotation} => {
            let doc = last_doc.take();
            ports.push(Port {doc, name, annotation})
        },
        TD::FunctionAnnotation{is_port:false, annotation,..}
        | TD::OperatorAnnotation(_, annotation) => {
            last_annotation = Some(annotation);
        },
        TD::FunctionDeclr{name, arguments, body} => {
            let doc = last_doc.take();
            let kind = FunctionKind::Regular;
            let annotation = last_annotation.take();
            functions.push(Function {
                annotation, doc, name, kind, arguments, body,
            })
        },
        TD::OperatorDeclr{name, arguments, body} => {
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

