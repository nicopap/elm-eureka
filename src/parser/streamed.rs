//! A lazily evaluated parser
use std::iter::SkipWhile;
use std::mem::replace;

use itertools::{multipeek,MultiPeek,Coalesce,Itertools};

use ::position::Iterplus;
use super::grammar::{parse_ModuleDeclr,parse_Import, parse_TopDeclr};
use super::tree;
use super::filter_indent::TokenIterator;
use ::tokens::ElmToken;
use ::lexer::{LexableIterator,Lexer};
use self::ElmToken::Newline;

macro_rules! not_match {
    ($to_match:pat) => (|x| match *x { $to_match => false, _ => true })
}
macro_rules! is_match {
    ($to_match:pat) => (|x| match x { $to_match => true, _ => false })
}

// NOTE: I use `appart_first_take` to keep the newline token at the beginning
// of the parser input, so it can be used for location tracking.
// This is so the TopDeclr parser can use the line information.
// There is multiple alternate implementations I concidered such as:
// * in `StageTopDeclrs`, pop the leading Newline of top declarations, use
//   it with the result of the old parser
//   * Needs refactoring the `StageTopDeclr` `next_step` function, must
//     adapt the `tree::TopDeclr` to accomodate for both the parser and the
//     "post-parser" modification of the data type.
//   * Needs to setup somehow the first Newline.
// * Add location tracking directly to the tokens (how every parsing libraries
//   does it)
//   * Attempted once, and gave up.
//
// The current solution has the advantage of being light on the line and logic
// changes necessary to update the code.
// Although, it might not  be the best in terms of performances.
macro_rules! as_token_span {
    ($input:expr, filter_newlines) => (
        $input
            .by_ref()
            .appart_first_take(not_match!(Newline(_,0)))
            .filter(|x| match *x {Newline(_,x) if x != 0 => false, _ => true})
            .enumerate()
            .map(|(i, token)| Ok((i, token, i+1)))
    );
    ($input:expr, filter_indent) => (
        $input
            .by_ref()
            .appart_first_take(not_match!(Newline(_,0)))
            .filter_indent()
            .enumerate()
            .map(|(i, token)| Ok((i, token, i+1)))
    )
}

// Look ahead in input two times, see what the token is, acts accordingly
// (if it is the one we expect, we $iffound, otherwise we $iflost)
macro_rules! ifahead2 {
    (($input:expr => $pred:pat) $iffound:block  else  $iffalse:block ) => ({
        $input.reset_peek();
        $input.peek();
        match $input.peek() {
            Some($pred) => { $input.reset_peek(); $iffound },
            _           => { $input.reset_peek(); $iffalse },
        }
    })
}

// wrappers to deal with lazy input stream reification.
type CoalSig<X> = fn(X, X)->Result<X, (X,X)>;
type OMG<I>=MultiPeek<Coalesce<
    SkipWhile<I, for<'r> fn(&'r ElmToken) -> bool >,
    CoalSig<ElmToken>
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
/// to *mark* the StreamParser `stage`. The `stage` is a
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
/// Currently, the StreamParser has the following stages:
///
/// 1. ModuleDeclr: `next_step` will consume the module
///    declaration.
///
/// 2. ModuleDoc: `next_step` will consume the module
///    doc string (if it exists).
///
/// 3. Imports: `next_step` will consume all the imports
///
/// 4. TopDeclrs: parses the rest of the file until EOF.
///    (currently only parses type declarations)
///
/// 5. EOF: Nothing more to be parsed.
struct StreamParser<I: Iterator<Item=ElmToken>, S = StageModuleDeclr> {
    input : OMG<I>,
    stage : S,
}

/// Converts a token stream into a parser
trait IntoParsed: Iterator<Item=ElmToken> {
    /// Add some processing to the input token stream and embed
    /// into the Iterator adaptator StreamParser at its initial
    /// stage.
    fn into_parsed(self) -> StreamParser<Self,StageModuleDeclr>
        where Self: Sized
    {
        use self::ElmToken::{Type, Name, Alias};

        let is_newline : fn(&ElmToken) -> bool = is_match!(&Newline(_,_));
        let remove_dup_newlines : CoalSig<ElmToken> = |prev,cur| {
            match (prev,cur) {
                (Newline(_,_), latest@Newline(_,_)) => Ok(latest),

                (Type, Name(ref content)) if content == "alias" =>
                    Err((Type, Alias)),

                (prev_, cur_) => Err((prev_, cur_)),
            }
        };
        StreamParser {
            input :
                multipeek(self.skip_while(is_newline)
                    .coalesce(remove_dup_newlines)),
            stage : StageModuleDeclr,
        }
    }
}
impl<T: Iterator<Item=ElmToken>> IntoParsed for T {}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageModuleDeclr> {
    fn next_step(mut self) -> StreamParser<I,StageModuleDoc> {
        let module_declr
            = parse_ModuleDeclr(as_token_span!(self.input, filter_newlines))
                .expect("Parsing error in module declaration");

        StreamParser {
            input : self.input,
            stage : StageModuleDoc{module_declr},
        }
    }
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageModuleDoc> {
    fn next_step(mut self) -> StreamParser<I,StageImports> {
        // Consumes the doc string if existant.
        ifahead2!((self.input => &ElmToken::DocComment(_)) {
            self.input.next().unwrap();
            let module_declr = self.stage.module_declr;
            let module_doc = match self.input.next() {
                Some(ElmToken::DocComment(content)) => Some(content),
                _ => None,
            };
            StreamParser {
                input: self.input,
                stage: StageImports{module_declr, module_doc},
            }
        } else {
            let module_declr = self.stage.module_declr;
            let module_doc = None;
            StreamParser {
                input: self.input,
                stage: StageImports{module_declr, module_doc},
            }
        })
    }
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageImports> {
    fn next_step(mut self) -> StreamParser<I,StageTopDeclrs> {
        let mut module_imports : Vec<tree::ElmImport> = Vec::new();
        loop {
            ifahead2!((self.input => &ElmToken::Import) {
                let cur_import
                    = parse_Import(as_token_span!(self.input, filter_newlines))
                        .expect("Parsing error in imports");

                module_imports.push(cur_import);
            } else { break })
        }
        let module_declr = self.stage.module_declr;
        let module_doc = self.stage.module_doc;
        StreamParser {
            input: self.input,
            stage: StageTopDeclrs{module_declr, module_doc, module_imports},
        }
    }
}


impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageTopDeclrs> {
    fn next_step(mut self) -> StreamParser<I,StageEof> {
        let mut top_levels = Vec::new();
        loop {
            self.input.reset_peek();
            match self.input.peek() {
                Some(&Newline(_,0)) => {
                    if self.input.peek() == None { break }
                    let top_declaration = match parse_TopDeclr(
                        as_token_span!(self.input, filter_indent)
                    ) {
                        Ok(val) => val,
                        Err(err) =>
                            panic!("{:?}: {:?}\ntoplevels: {:?}\nself:{:?}, {:?}",
                                   err,
                                   self.input.collect::<Vec<_>>(),
                                   top_levels,self.stage.module_declr,
                                   self.stage.module_doc),
                    };
                    top_levels.push(top_declaration);
                },
                Some(_) =>
                    panic!("error in toplevel parsing:\n\
                            encountered {:?} while expecting a top newline \
                            token \n\
                           {:?}\ntoplevels: {:?}\nself:{:?}, {:?}",
                           self.input.next(),
                           self.input.collect::<Vec<_>>(),
                           top_levels,self.stage.module_declr,
                           self.stage.module_doc),
                None => break,
            }
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
    let (list_ports, infixities, functions, types)
        = tree::into_tree(top_levels.into_iter());

    let ports = if list_ports.is_empty() { None } else { Some(list_ports) };
    tree::ElmModule {
        name, exports, doc, imports, types, functions, infixities, ports
    }
}

enum ParserState<I:Iterator<Item=ElmToken>> {
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
    pub fn exposed_symbols<'a>(&'a mut self) -> Vec<&'a str> {
        use self::ParserState as PS;
        use self::tree::ExportEntry as EE;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        let exports =
            match self.0 {
                PS::ModuleDoc(ref parser) => &parser.stage.module_declr.exports,
                PS::Imports(ref parser) => &parser.stage.module_declr.exports,
                PS::TopDeclrs(ref parser) => &parser.stage.module_declr.exports,
                PS::FullyParsed(ref module_tree) => &module_tree.exports,
                _ => unreachable!()
            };
        match *exports {
            tree::ExportList::Unqualified => vec![],
            tree::ExportList::List(ref entries) => {
                entries
                    .iter()
                    .map(|entry| match *entry {
                        EE::Name(ref name) => name.as_ref(),
                        EE::Operator(ref op) => op.as_ref(),
                        EE::WithConstructors(ref name,_) => name.as_ref(),
                        EE::WithAllConstructors(ref name) => name.as_ref(),
                    })
                    .collect::<Vec<&'a str>>()
            },
        }
    }

    /// Returns the export list of the module. Evaluating it if needed
    pub fn module_exports<'a>(&'a mut self) -> &'a tree::ExportList {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        match self.0 {
            PS::ModuleDoc(ref parser) => &parser.stage.module_declr.exports,
            PS::Imports(ref parser) => &parser.stage.module_declr.exports,
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
    pub fn module_name<'a>(&'a mut self) -> &'a String {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        match self.0 {
            PS::ModuleDoc(ref parser) => &parser.stage.module_declr.name,
            PS::Imports(ref parser) => &parser.stage.module_declr.name,
            PS::TopDeclrs(ref parser) => &parser.stage.module_declr.name,
            PS::FullyParsed(ref module_tree) => &module_tree.name,
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
    pub fn module_doc<'a>(&'a mut self) -> &'a Option<String> {
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
    pub fn imports<'a>(&'a mut self) -> &'a [tree::ElmImport] {
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
    pub fn functions<'a>(&'a mut self) -> &'a [tree::FunctionDeclaration] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::FullyParsed(ref module_tree) => &module_tree.functions,
            _ => unreachable!()
        }
    }

    /// Returns the list of type declarations. The
    /// whole file is parsed in order to to generate that list
    pub fn types<'a>(&'a mut self) -> &'a [tree::TypeDeclaration] {
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

