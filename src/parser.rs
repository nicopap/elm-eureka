//! An efficient implementation of an elm parser

use std::iter::{Peekable,SkipWhile};

use itertools::{Coalesce,Itertools};

use ast;
use elm_parse::{parse_ModuleDeclr,parse_Import};
use tokens::{ElmToken,LexError};
use self::ElmToken::{DocComment, Newline};

type SpanToken = Result<(usize, ElmToken, usize), LexError>;

// wrappers to deal with lazy input stream reification.
type CoalSig<X> = fn(X, X)->Result<X, (X,X)>;
type OMG<I>=Peekable<Coalesce<
    SkipWhile< I,for<'r> fn(&'r ElmToken)->bool >,
    CoalSig<ElmToken>
>>;

struct StageModuleDeclr;
struct StageModuleDoc {
    pub module_declr: ast::ModuleDeclr,
}
struct StageImports {
    pub module_declr: ast::ModuleDeclr,
    pub module_doc: Option<String>,
}

// Possible alternate implementation to keep in mind:
// Have an iterator over the Imports rather than
// collect them directly into a Vec (but fuck that!)
struct StageTopDeclrs {
    pub module_declr: ast::ModuleDeclr,
    pub module_doc: Option<String>,
    pub module_imports: Vec<ast::ElmImport>,
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
/// previous stage to *yield* a new one.
///
/// The stages that divides a source file were choosen
/// by ease of implementation and mean of use:
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
/// 4. TopDeclrs: does not implement `next_step`.
///
/// Further stages will be added as I refine the design
/// of the parser.
struct StreamParser<I: Iterator<Item=ElmToken>, Stage> {
    input : OMG<I>,
    stage : Stage,
}

macro_rules! not_match {
    ($to_match:pat) => ( |x| match *x { $to_match => false, _ => true })
}
macro_rules! is_match {
    ($to_match:pat) => ( |x| match x { $to_match => true, _ => false })
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I, StageModuleDeclr> {
    /// Add some processing to the input token stream and embed
    /// into the Iterator adaptator StreamParser at its initial
    /// stage.
    pub fn new(input : I) -> StreamParser<I, StageModuleDeclr> {
        let is_newline : fn(&ElmToken)->bool = is_match!(&Newline(_,_));
        let remove_dup_newlines : CoalSig<ElmToken> = |prev,cur| {
            match (prev,cur) {
                (Newline(_,0), Newline(x,0)) => Ok(Newline(x,0)),
                (prev_, cur_) => Err((prev_, cur_)),
            }
        };
        StreamParser {
            input :
                input
                    .skip_while(is_newline)
                    .coalesce(remove_dup_newlines)
                    .peekable(),
            stage : StageModuleDeclr,
        }
    }
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageModuleDeclr> {
    fn next_step(mut self) -> StreamParser<I,StageModuleDoc> {
        // Consumes the module declaration, also consumes the next newline.
        let declr_stream : Vec<SpanToken> =
            self.input
                .by_ref()
                .take_while(not_match!(ElmToken::Newline(_,0)))
                .filter(not_match!(ElmToken::Newline(_,_)))
                .enumerate()
                .map(|(i, token)| Ok((i, token, i+1)))
                .collect();
        let module_declr = parse_ModuleDeclr(declr_stream).expect("Parsing Error");
        StreamParser {
            input : self.input,
            stage : StageModuleDoc{module_declr},
        }
    }
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageModuleDoc> {
    fn next_step(mut self) -> StreamParser<I,StageImports> {
        // Consumes the doc string if existant.
        match self.input.peek() {
            Some(&DocComment(_)) => {
                let module_declr = self.stage.module_declr;
                let module_doc = match self.input.next() {
                    Some(DocComment(content)) => Some(content),
                    _ => None,
                };
                self.input.next().expect("Sourcefile with only doc and module declr");
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
            },
        }
    }
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageImports> {
    fn next_step(mut self) -> StreamParser<I,StageTopDeclrs> {
        let mut module_imports : Vec<ast::ElmImport> = Vec::new();
        while self.input.peek() == Some(&ElmToken::Import) {
            let import_stream : Vec<SpanToken> =
                self.input
                    .by_ref()
                    .take_while(not_match!(ElmToken::Newline(_,0)))
                    .filter(not_match!(ElmToken::Newline(_,_)))
                    .enumerate()
                    .map(|(i, token)| Ok((i, token, i+1)))
                    .collect();
            let cur_import = parse_Import(import_stream).expect("Parsing Error");
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

#[derive(Debug)]
pub struct Parser {
    module_declr: ast::ModuleDeclr,
    module_doc: Option<String>,
    imports: Vec<ast::ElmImport>,
}

impl Parser {
    pub fn new<I>(input: I) -> Parser where I:Iterator<Item=ElmToken> {
        let stream_parsed =
            StreamParser::new(input)
                .next_step()
                .next_step()
                .next_step()
                .stage;
        Parser {
            module_declr: stream_parsed.module_declr,
            module_doc: stream_parsed.module_doc,
            imports: stream_parsed.module_imports,
        }
    }
    pub fn get_module_doc(&self) -> &Option<String> {
        &self.module_doc
    }
    pub fn get_module_declr(&self) -> &ast::ModuleDeclr {
        &self.module_declr
    }
    pub fn get_imports(&self) -> &Vec<ast::ElmImport> {
        &self.imports
    }
}
