//! A lazily evaluated parser
use std::iter::{SkipWhile,Peekable};
use std::mem::replace;

use itertools::{Coalesce,Itertools};

use super::grammar::{parse_ModuleDeclr,parse_Import, parse_TopDeclr};
use super::tree;
use super::filter_indent::TokenIterator;
use ::tokens::ElmToken;
use ::lexer::{LexableIterator,Lexer};
use self::ElmToken::Newline;


pub type Loc<X> = (u32,X);

macro_rules! matches {
    (not $to_match:pat) => (|x| match *x { $to_match => false, _ => true });
    ($to_match:pat)     => (|x| match *x { $to_match => true, _ => false });
}

// Shorthand to convert a simple Loc<ElmToken> stream into
// something that the LALRPOP-generated parser accepts
macro_rules! lalrpopify {
    (@ filter_nl $in:ident)=>($in.filter(matches!(not (_,Newline(_)))));
    (@ filter_indent $in:ident)  =>($in.filter_indent());
    ($input:expr, $kind:ident) => ({
        let line_cut = $input
            .by_ref()
            .take_while(matches!(not (_,Newline(0))));
        lalrpopify!(@ $kind line_cut)
            .map(|(line,token)| Ok(( line, token, line )))
    });
}

// wrappers to deal with lazy input stream reification.
type CoalSig<X> = fn(X, X)->Result<X, (X,X)>;
type OMG<I>=Peekable<Coalesce<
    SkipWhile<I,for<'r> fn(&'r Loc<ElmToken>) -> bool>,
    CoalSig<Loc<ElmToken>>
>>;

struct StageModuleDeclr;
struct StageModuleDoc {
    module_declr: tree::ModuleDeclr,
}
struct StageImports {
    module_declr: tree::ModuleDeclr,
    module_doc: Option<String>,
}

// Possible alternate implementation to keep in mind:
// Have an iterator over the Imports rather than
// collect them directly into a Vec (but fuck that!)
struct StageTopDeclrs {
    module_declr: tree::ModuleDeclr,
    module_doc: Option<String>,
    module_imports: Vec<tree::ElmImport>,
}

struct StageEof {
    module_declr: tree::ModuleDeclr,
    module_doc: Option<String>,
    module_imports: Vec<tree::ElmImport>,
    top_levels: Vec<tree::TopDeclr>,
}

/// A parser with different levels of processing state.
///
/// We accept an iterator over tokens representing a lexed
/// elm source file. We only evaluate a token when we need
/// it for parsing. In sum this is a lazy parser.
///
/// To achieve laziness, efficiency and safety, I use typing
/// to *mark* the `StreamParser` `stage`. The `stage` is a
/// structure that holds what part of the tree is currently
/// parsed and hints at what is remaining to be parsed.
///
/// To express the transition between stages (progress),
/// I implement `next_step` for a specialized
/// `StreamParser` over a given *stage* and return a
/// `StreamParser` with its new *stage*. This is possible
/// thanks to Rust's affine typing: we *consume* the
/// previous stage to *yield* a new one. The stages
/// are the following:
///
/// * It uses the newline at column 0 token to split
///   the input into meaningfull parsable units.
///
/// * It divides the parser in logic units that follows
///   what information you'd want to extract out of a file.
///
/// Currently, the `StreamParser` has the following stages:
///
/// 1. `ModuleDeclr`: `next_step` will consume the module
///    declaration.
///
/// 2. `ModuleDoc`: `next_step` will consume the module
///    doc string (if it exists).
///
/// 3. `Imports`: `next_step` will consume all the imports
///
/// 4. `TopDeclrs`: parses the rest of the file until EOF.
///    (currently only parses type declarations)
///
/// 5. EOF: Nothing more to be parsed.
struct StreamParser<I: Iterator<Item=Loc<ElmToken>>, S = StageModuleDeclr> {
    input : OMG<I>,
    stage : S,
}

/// Converts a token stream into a parser
trait IntoParsed: Iterator<Item=Loc<ElmToken>> {
    /// Add some processing to the input token stream and embed
    /// into the Iterator adaptator StreamParser at its initial
    /// stage.
    fn into_parsed(self) -> StreamParser<Self,StageModuleDeclr>
        where Self: Sized
    {
        use self::ElmToken::{Type, Name, Alias};

        let is_newline: fn(&Loc<ElmToken>)->bool = matches!((_,Newline(_)));
        let remove_dup_newlines: CoalSig<Loc<ElmToken>> = |prev,cur| {
            match (prev,cur) {
                ((_,Newline(_)), latest@(_,Newline(_))) => Ok(latest),

                ((tline,Type), (aline,Name(ref content))) if content == "alias" =>
                    Err(( (tline,Type), (aline,Alias) )),

                (prev_, cur_) => Err((prev_, cur_)),
            }
        };
        StreamParser {
            input :
                self.skip_while(is_newline)
                    .coalesce(remove_dup_newlines)
                    .peekable(),
            stage : StageModuleDeclr,
        }
    }
}
impl<T: Iterator<Item=Loc<ElmToken>>> IntoParsed for T {}

impl<I:Iterator<Item=Loc<ElmToken>>> StreamParser<I,StageModuleDeclr> {
    fn next_step(mut self) -> StreamParser<I,StageModuleDoc> {
        let module_declr
            = parse_ModuleDeclr(lalrpopify!(self.input, filter_nl))
                .expect("Parsing error in module declaration");

        StreamParser {
            input : self.input,
            stage : StageModuleDoc{module_declr},
        }
    }
}

impl<I:Iterator<Item=Loc<ElmToken>>> StreamParser<I,StageModuleDoc> {
    fn next_step(mut self) -> StreamParser<I,StageImports> {
        // Consumes the module doc if existant.
        match self.input.peek() {
            Some(&(_, ElmToken::DocComment(_))) => {
                let module_declr = self.stage.module_declr;
                let module_doc = match self.input.next() {
                    Some((_,ElmToken::DocComment(content))) =>
                        Some(content),
                    anyelse =>
                        panic!("read a DocComment with a peek, \
                                now, I get something different!
                                : {:?}", anyelse),
                };
                match self.input.next() {
                    Some((_, ElmToken::Newline(_))) => {},
                    None =>
                        panic!("There is no source following a module doc \
                                comment! This is malformed code I should \
                                be able to handle but doens't yet."),
                    anyelse =>
                        panic!("A new line should follow doc comment, yet \
                               something else was there instead \
                                : {:?}", anyelse),
                };
                StreamParser {
                    input: self.input,
                    stage: StageImports{module_declr, module_doc},
                }
            },
            _ => {
                let module_declr = self.stage.module_declr;
                let module_doc = None;
                StreamParser {
                    input: self.input,
                    stage: StageImports{module_declr, module_doc},
                }
            }
        }
    }
}

impl<I:Iterator<Item=Loc<ElmToken>>> StreamParser<I,StageImports> {
    fn next_step(mut self) -> StreamParser<I,StageTopDeclrs> {
        let mut module_imports : Vec<tree::ElmImport> = Vec::new();
        while let Some(&(_, ElmToken::Import)) = self.input.peek() {
            let cur_import
                = parse_Import(lalrpopify!(self.input, filter_nl))
                    .expect("Parsing error in imports");
            module_imports.push(cur_import);
        }
        let module_declr = self.stage.module_declr;
        let module_doc = self.stage.module_doc;
        StreamParser {
            input: self.input,
            stage: StageTopDeclrs{module_declr, module_doc, module_imports},
        }
    }
}


impl<I:Iterator<Item=Loc<ElmToken>>> StreamParser<I,StageTopDeclrs> {
    fn next_step(mut self) -> StreamParser<I,StageEof> {
        let mut top_levels = Vec::new();
        loop {
            match parse_TopDeclr(lalrpopify!(self.input, filter_indent)) {
                Ok(val) => top_levels.push(val),
                Err(_) => break // TODO: handle this more gracefully
            };
        }
        let module_declr = self.stage.module_declr;
        let module_doc = self.stage.module_doc;
        let module_imports = self.stage.module_imports;
        StreamParser {
            input: self.input,
            stage: StageEof {
                module_declr, module_doc, module_imports, top_levels,
            },
        }
    }
}

fn into_tree(parser: StageEof) -> tree::ElmModule {
    let StageEof{module_declr,module_doc,module_imports,top_levels}=parser;
    let tree::ModuleDeclr {name, exports} = module_declr;
    let doc = module_doc;
    let imports = module_imports;
    let tree::CoalecedTopDeclr {
        ports:list_ports, infixities, functions, types
    } = tree::into_tree(top_levels.into_iter());

    let ports = if list_ports.is_empty() { None } else { Some(list_ports) };
    tree::ElmModule {
        name, exports, doc, imports, types, functions, infixities, ports
    }
}

enum ParserState<I:Iterator<Item=Loc<ElmToken>>> {
    ModuleDeclr(StreamParser<I,StageModuleDeclr>),
    ModuleDoc(StreamParser<I,StageModuleDoc>),
    Imports(StreamParser<I,StageImports>),
    TopDeclrs(StreamParser<I,StageTopDeclrs>),
    FullyParsed(tree::ElmModule),
    Nothing,
}

enum ParserStage {
    ModuleDoc,
    Imports,
    TopDeclrs,
    FullyParsed,
}

/// A lazy parser.
///
/// It only consumes the characters needed to return the values requested
/// by the methods called on it.
///
/// The getter functions take a mutable borrow because it may need to
/// mutate in place the parser in order to evaluate (and remember) a
/// part of the parse tree.
///
/// Inside notes: the Iterator is lexed and parsed progressively, as
/// needed.
pub struct Parser<I:Iterator<Item=char>>(ParserState<Lexer<I>>);

impl<I> Parser<I>
    where I:Iterator<Item=char>
{
    /// Create a lazily evaluating parser.
    ///
    /// # Example
    /// ```rust
    /// # #[allow(unused_variables)]
    /// # use elm_eureka::Parser;
    /// let elm_source = r#"module A exposing (f,X)
    /// {-| Module doc -}
    /// import A.B.C as C
    ///
    /// f : Int -> { f1: Int, f2: List Float }
    /// f x = C.m <| x
    ///
    /// type alias X a b c = a -> b -> c
    /// "#;
    /// let mut parser = Parser::new(elm_source.chars());
    /// let parse_tree = parser.into_parse_tree();
    /// ```
    pub fn new(input: I) -> Parser<I> {
        Parser(ParserState::ModuleDeclr(input.lex().into_parsed()))
    }

    /// Parse the file up to the given stage.
    fn evaluate_up_to(&mut self, stage: ParserStage) {
        use self::ParserState as ST;
        use self::ParserStage as SG;
        let old_stage = replace(&mut self.0, ST::Nothing);
        macro_rules! next_n {
            (+   ; $to_next:expr) => ( $to_next.next_step() );
            (++  ; $to_next:expr) => (next_n!(+  ;$to_next.next_step()));
            (+++ ; $to_next:expr) => (next_n!(++ ;$to_next.next_step()));
            (++++; $to_next:expr) => (next_n!(+++;$to_next.next_step()));
        }
        macro_rules! fully_parsed {
            ($arg:expr) => ( ST::FullyParsed(into_tree($arg.stage)))
        }
        match (stage, old_stage) {
            (SG::ModuleDoc, ST::ModuleDeclr(parser)) =>
                self.0 = ST::ModuleDoc(next_n!(+   ; parser)),
            (SG::ModuleDoc, anyelse) => self.0 = anyelse,

            (SG::Imports, ST::ModuleDeclr(parser)) =>
                self.0 = ST::Imports(next_n!(++    ; parser)),
            (SG::Imports, ST::ModuleDoc(parser)) =>
                self.0 = ST::Imports(next_n!(+     ; parser)),
            (SG::Imports, anyelse) => self.0 = anyelse,

            (SG::TopDeclrs, ST::ModuleDeclr(parser)) =>
                self.0 = ST::TopDeclrs(next_n!(+++ ; parser)),
            (SG::TopDeclrs, ST::ModuleDoc(parser)) =>
                self.0 = ST::TopDeclrs(next_n!(++  ; parser)),
            (SG::TopDeclrs, ST::Imports(parser)) =>
                self.0 = ST::TopDeclrs(next_n!(+   ; parser)),
            (SG::TopDeclrs, anyelse) => self.0 = anyelse,

            (SG::FullyParsed, ST::ModuleDeclr(parser)) =>
                self.0 = fully_parsed!(next_n!(++++; parser)),
            (SG::FullyParsed, ST::ModuleDoc(parser)) =>
                self.0 = fully_parsed!(next_n!(+++ ; parser)),
            (SG::FullyParsed, ST::Imports(parser)) =>
                self.0 = fully_parsed!(next_n!(++  ; parser)),
            (SG::FullyParsed, ST::TopDeclrs(parser)) =>
                self.0 = fully_parsed!(next_n!(+   ; parser)),
            (SG::FullyParsed, anyelse) => self.0 = anyelse,
        }
    }

    /// Returns the names that the module exposes. Evaluating it if needed
    /// NB: Currently returns an empty list if the export is unqualified,
    /// this is also true for type constructors.
    ///
    /// # Example
    /// ```rust
    /// # #[allow(unused_variables)]
    /// # use elm_eureka::Parser;
    /// let elm_source = r#"module A exposing (f,X)
    /// {-| Module doc -}
    /// import A.B.C as C
    ///
    /// f : Int -> { f1: Int, f2: List Float }
    /// f x = C.m <| x
    ///
    /// type alias X a b c = a -> b -> c
    /// "#;
    ///
    /// let mut parser = Parser::new(elm_source.chars());
    /// let symbols = parser.exposed_symbols();
    /// assert_eq!(symbols, vec!["f", "X"]);
    /// ```
    pub fn exposed_symbols(&mut self) -> Vec<&str> {
        use self::ParserState as PS;
        use self::tree::ExportEntry as EE;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        let exports =
            match self.0 {
                PS::ModuleDeclr(_) => unreachable!(),
                PS::ModuleDoc(ref parser) => &parser.stage.module_declr.exports,
                PS::Imports(ref parser) =>   &parser.stage.module_declr.exports,
                PS::TopDeclrs(ref parser) => &parser.stage.module_declr.exports,
                PS::FullyParsed(ref module_tree) => &module_tree.exports,
                PS::Nothing => unreachable!(),
            };
        match *exports {
            tree::ExportList::Unqualified => { vec![]
            },
            tree::ExportList::List(ref entries) => {
                entries
                    .iter()
                    .map(|entry| match *entry {
                        EE::WithConstructors(ref name,_)
                        | EE::Name(ref name)
                        | EE::WithAllConstructors(ref name)
                        | EE::Operator(ref name) => name.as_ref(),
                    })
                    .collect::<Vec<&str>>()
            },
        }
    }

    /// Returns the export list of the module. Evaluating it if needed
    pub fn module_exports(&mut self) -> &tree::ExportList {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        match self.0 {
            PS::ModuleDoc(ref parser) => &parser.stage.module_declr.exports,
            PS::Imports(ref parser) =>   &parser.stage.module_declr.exports,
            PS::TopDeclrs(ref parser) => &parser.stage.module_declr.exports,
            PS::FullyParsed(ref module_tree) => &module_tree.exports,
            _ => unreachable!()
        }
    }

    /// Returns the name of the module. Evaluating it if needed
    ///
    /// # Example
    /// ```rust
    /// # #[allow(unused_variables)]
    /// # use elm_eureka::Parser;
    /// let elm_source = r#"module My.Great.Module exposing (f,X)
    /// {-| Module doc -}
    /// import A.B.C as C
    ///
    /// f : Int -> { f1: Int, f2: List Float }
    /// f x = C.m <| x
    ///
    /// type alias X a b c = a -> b -> c
    /// "#;
    ///
    /// let mut parser = Parser::new(elm_source.chars());
    /// let name = parser.module_name();
    /// assert_eq!(name, &String::from("My.Great.Module"));
    /// ```
    pub fn module_name(&mut self) -> &str {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        match self.0 {
            PS::ModuleDoc(ref parser) => parser.stage.module_declr.name.as_ref(),
            PS::Imports(ref parser) =>   parser.stage.module_declr.name.as_ref(),
            PS::TopDeclrs(ref parser) => parser.stage.module_declr.name.as_ref(),
            PS::FullyParsed(ref module_tree) => module_tree.name.as_ref(),
            _ => unreachable!()
        }
    }

    /// Returns the export list of the module. Evaluating it if
    /// needed
    ///
    /// # Example
    /// ```rust
    /// # #[allow(unused_variables)]
    /// # use elm_eureka::Parser;
    /// let elm_source = r#"module My.Great.Module exposing (f,X)
    /// {-| Some useful informations
    /// -}
    /// import A.B.C as C
    ///
    /// f : Int -> { f1: Int, f2: List Float }
    /// f x = C.m <| x
    ///
    /// type alias X a b c = a -> b -> c
    /// "#;
    ///
    /// let mut parser = Parser::new(elm_source.chars());
    /// let docs = parser.module_doc();
    /// assert_eq!(docs, &Some(String::from(" Some useful informations\n")));
    /// ```
    pub fn module_doc(&mut self) -> &Option<String> {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::Imports);
        match self.0 {
            PS::Imports(ref parser) => &parser.stage.module_doc,
            PS::TopDeclrs(ref parser) => &parser.stage.module_doc,
            PS::FullyParsed(ref module_tree) => &module_tree.doc,
            _ => unreachable!()
        }
    }

    /// Returns the list of import declarations. Evaluating it if needed
    pub fn imports(&mut self) -> &[tree::ElmImport] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::TopDeclrs);
        match self.0 {
            PS::TopDeclrs(ref parser) => &parser.stage.module_imports,
            PS::FullyParsed(ref module_tree) => &module_tree.imports,
            _ => unreachable!()
        }
    }

    /// Returns the list of function declarations. The
    /// whole file is parsed in order to to generate that list
    pub fn functions(&mut self) -> &[tree::FunctionDeclaration] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::FullyParsed(ref module_tree) => &module_tree.functions,
            _ => unreachable!()
        }
    }

    /// Returns the list of type declarations. The
    /// whole file is parsed in order to to generate that list
    pub fn types(&mut self) -> &[tree::TypeDeclaration] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::FullyParsed(ref module_tree) => &module_tree.types,
            _ => unreachable!()
        }
    }

    /// Parse the whole file and return the complete parse tree.
    pub fn into_parse_tree(mut self) -> tree::ElmModule {
        use self::ParserState as PS;

        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::FullyParsed(module_tree) => module_tree,
            _ => unreachable!()
        }
    }
}

