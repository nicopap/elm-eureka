//! An efficient implementation of an elm parser

use std::iter::{Peekable,SkipWhile};
use std::fmt;

use itertools::{Coalesce,Itertools};
use either::Either;
use either::Either::Right;

use ast;
use elm_parse::{parse_ModuleDeclr,parse_Import, parse_TopDeclr};
use tokens::{ElmToken,LexError};
use self::ElmToken::{DocComment, Newline, Name, LParens, Port};

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

struct StageEof {
    pub module_declr: ast::ModuleDeclr,
    pub module_doc: Option<String>,
    pub module_imports: Vec<ast::ElmImport>,
    pub top_levels: Vec<Either<Vec<SpanToken>,ast::TopDeclr>>,
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
/// 4. TopDeclrs: parses the rest of the file until EOF.
///    (currently only parses type declarations)
///
/// 5. EOF: Nothing more to be parsed.
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
                (Newline(_,_), latest@Newline(_,_)) => Ok(latest),
                (ElmToken::Type, Name(ref content)) if content == "alias" =>
                    Err((ElmToken::Type, ElmToken::Alias)),
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
                .take_while(not_match!(Newline(_,0)))
                .filter(not_match!(Newline(_,_)))
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
            let import_stream =
                self.input
                    .by_ref()
                    .take_while(not_match!(Newline(_,0)))
                    .filter(not_match!(Newline(_,_)))
                    .enumerate()
                    .map(|(i, token)| Ok((i, token, i+1)));
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

#[derive(PartialEq, Copy, Clone)] enum IndentEntry {
    Let(i16),
    Case(i16),
    // The delimiters are `let .. in`, `( .. )`, `[ .. ]`, `{ .. }`,
    // `if .. then .. else`.
    // Keeping track of them helps telling where to insert the `endcase`
    // tokens.
    Delimiter,
}

impl fmt::Debug for IndentEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IndentEntry::Let(indent) => write!(f, "l{}", indent),
            IndentEntry::Case(indent) => write!(f, "c{}", indent),
            IndentEntry::Delimiter => write!(f, "del"),
        }
    }
}
#[derive(PartialEq, Debug, Copy, Clone)] enum IndentTrigger {Let, Of}

/// An iterator adaptator that turns Newline tokens into meaningfull
/// indentation for the parser to consume.
///
/// The FilterIndent keeps as internal state the indent stack. The
/// indent stack keeps track of meaningfull indentations for when
/// they need to be aligned, and for inserting "endcase" tokens when
/// necessary (nested `case` expressions). The `indent_stack` not only
/// keeps track of what "meaningfull" indentation lines to look for, but
/// also of enclosing delimiters such as `()`, `[]` or `{}`. This is so
/// we do not insert `endcase` tokens at the wrong place.
#[derive(Debug)]
struct FilterIndent<I:Iterator<Item=ElmToken>> {
    input: I,
    // Holds information about the levels of indentation we are in, and
    // if there is "enclosing" expressions that renders `Of` ending moot.
    indent_stack: Vec<IndentEntry>,
    // If a keyword into an expression needing indentation is found, put
    // it there so at the next newline we know what to do
    indent_trigger: Option<IndentTrigger>,
    // A buffer to hold multiple tokens we want to emit later.
    buffer_stack: Vec<ElmToken>,
}

impl<I:Iterator<Item=ElmToken>> FilterIndent<I> {
    // Pop `indent_stack`, closing off unfinished Case expressions by
    // pushing `Endcase` in `buffer_stack`. `indent_stack` is poped until
    // given `entry` is found, the corresponding item is poped from the
    // stack.
    // We only push `Endcase` when there is two succeeding `Of` sources.
    // This follows the grammatical rules of only closing `case` expressions
    // with an `endcase` if the `case` are nested or nested within open
    // expressions.
    fn pop_indents_to(&mut self, entry: IndentEntry) {
        let mut last_indenter_is_case = false;
        loop { match self.indent_stack.pop() {
            None => return,
            Some(IndentEntry::Case(indent_level)) if last_indenter_is_case => {
                self.buffer_stack.push(ElmToken::Endcase);
                if IndentEntry::Case(indent_level) == entry { return }
            },
            Some(IndentEntry::Case(indent_level)) /* otherwise */ => {
                last_indenter_is_case = true;
                if IndentEntry::Case(indent_level) == entry { return }
            },
            Some(indenter) if indenter == entry => return,
            Some(_) /* otherwise */ => {
                last_indenter_is_case = false;
            },
        } }
    }

    // Finds the corresponding indent in `indent_stack`.
    // pop `indent_stack` and inserts token in `buffer_stack` accordingly
    //
    // Ideally this should be inlined in the only place where calling this
    // function is relevent (in source code)
    fn locate_indent(&mut self, indent_level: i16) {
        // This function works in two steps:
        // 1. detects if an entry in the `indent_stack` has the corresponding
        //    `indent_level`.
        // 2. If not, returns.
        //    otherwise pops `indent_stack` up to the found `indent_level`
        use self::IndentEntry::{Case,Let};
        if self.indent_stack.is_empty() { return }
        let stack_last = self.indent_stack.len() - 1;
        let mut idx = stack_last;
        match loop {
            // I mean seriously, this is bound checked 1000%
            let current_indenter = self.indent_stack.get(idx).unwrap();
            match *current_indenter {
                Case(indent) if indent == indent_level => {
                    self.buffer_stack.push(ElmToken::CaseIndent);
                    break Some(idx)
                },
                Let(indent) if indent == indent_level => {
                    self.buffer_stack.push(ElmToken::LetIndent);
                    break Some(idx)
                },
                _ => {},
            }
            if idx == 0 { break None }
            idx -= 1;
        } { // 2.
            None => return,
            Some(indenter_location) => {
                let pop_count = stack_last - indenter_location;
                let mut last_indenter_is_case = false;
                // Pop values over the indentation we found (leaving
                // the one we found)
                for _ in 0..pop_count {
                    match self.indent_stack.pop().unwrap() {
                        Case(_) if last_indenter_is_case => {
                            self.buffer_stack.push(ElmToken::Endcase);
                        },
                        Case(_) => last_indenter_is_case = true,
                        _ => last_indenter_is_case = false,
                    }
                }
                // Cleanup if we popped a case when we are aligned to a case
                if last_indenter_is_case
                   && self.indent_stack.last() == Some(&Case(indent_level))
                {
                    self.buffer_stack.push(ElmToken::Endcase)
                }
            },
        }
    }
}

impl<I:Iterator<Item=ElmToken>> Iterator for FilterIndent<I> {
    type Item=ElmToken;
    // TODO: keep track of location after the `let` keyword if
    // the next Token doesn't start at a new line
    fn next(&mut self) -> Option<ElmToken> {
        use self::ElmToken::*;

        if let Some(token) = self.buffer_stack.pop() { return Some(token) }
        match self.input.next()? { token => match token {
            LParens | LBrace | LBracket | If => {
                self.indent_stack.push(IndentEntry::Delimiter);
                return Some(token);
            },
            Let => {
                self.indent_trigger = Some(IndentTrigger::Let);
                self.indent_stack.push(IndentEntry::Delimiter);
                return Some(Let);
            },
            Of => {
                self.indent_trigger = Some(IndentTrigger::Of);
                return Some(Of);
            },
            RParens | RBrace | RBracket | Else => {
                self.buffer_stack.push(token);
                self.pop_indents_to(IndentEntry::Delimiter);
            },
            In => {
                self.indent_trigger = None;
                self.buffer_stack.push(In);
                self.pop_indents_to(IndentEntry::Delimiter);
            },
            Newline(_, column) => {
                match self.indent_trigger {
                    Some(IndentTrigger::Let) => {
                        self.indent_trigger = None;
                        self.indent_stack.push(IndentEntry::Let(column));
                    },
                    Some(IndentTrigger::Of) => {
                        self.indent_trigger = None;
                        self.indent_stack.push(IndentEntry::Case(column));
                    },
                    None => {
                        self.locate_indent(column);
                    },
                }
            },
            any_other => {
                return Some(any_other);
            },
        } }
        // if the match expression didn't return, we fall through to a
        // recursive call to self. Note that this is definitively equivalent
        // to a while loop.
        self.next()
    }
}

trait TokenIterator: Iterator<Item=ElmToken> {
    fn filter_indent(self) -> FilterIndent<Self> where Self: Sized {
        FilterIndent {
            input : self,
            indent_stack : Vec::new(),
            indent_trigger : None,
            buffer_stack: Vec::new(),
        }
    }
}

impl<T: Iterator<Item=ElmToken>> TokenIterator for T {}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageTopDeclrs> {
    fn next_step(mut self) -> StreamParser<I,StageEof> {
        // TODO: this is flawed because it doesn't capture lazily the
        // tokens for a type declaration.
        let mut top_levels = Vec::new();
        loop {
            match self.input.peek() {
                Some(&Name(_)) | Some(&LParens) | Some(&Port) => {
                    let preparsed_tokens =
                        self.input
                            .by_ref()
                            .take_while(not_match!(ElmToken::Newline(_,0)))
                            .filter_indent()
                            .enumerate()
                            .map(|(i, token)| Ok((i, token, i+1)));
                    top_levels.push(Right(
                        parse_TopDeclr(preparsed_tokens)
                            .expect("Function Parsing Error")
                    ));
                },
                Some(_) => {
                    let toplevel_stream =
                        self.input
                            .by_ref()
                            .take_while(not_match!(ElmToken::Newline(_,0)))
                            .filter(not_match!(ElmToken::Newline(_,_)))
                            .enumerate()
                            .map(|(i, token)| Ok((i, token, i+1)));
                    top_levels.push(Right(
                        parse_TopDeclr(toplevel_stream)
                            .expect("Parsing Error")
                    ));
                },
                None  => {
                    break;
                },
            }
        }
        let module_declr = self.stage.module_declr;
        let module_doc = self.stage.module_doc;
        let module_imports = self.stage.module_imports;
        StreamParser {
            input: self.input,
            stage: StageEof {
                module_declr,
                module_doc,
                module_imports,
                top_levels,
            },
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    module_declr: ast::ModuleDeclr,
    module_doc: Option<String>,
    imports: Vec<ast::ElmImport>,
    top_levels: Vec<ast::TopDeclr>,
}

/// This needs a lot of work. Planned features:
///
/// * Lazily evaluate the source file rather than collect
///   preemptively all the AST
impl Parser {
    pub fn new<I>(input: I) -> Parser where I:Iterator<Item=ElmToken> {
        let stream_parsed =
            StreamParser::new(input)
                .next_step().next_step().next_step().next_step()
                .stage;
        let proc_top_levels =
            stream_parsed.top_levels
                .into_iter()
                .map(Either::right)
                .flat_map(|op| op.into_iter())
                .collect();
        Parser {
            module_declr: stream_parsed.module_declr,
            module_doc: stream_parsed.module_doc,
            imports: stream_parsed.module_imports,
            top_levels: proc_top_levels,
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
    pub fn get_types(&self) -> (Vec<&ast::TypeAlias>, Vec<&ast::TypeDeclr>) {
        use ast::TopDeclr::{TypeAlias,TypeDeclr};
        let type_aliases =
            self.top_levels.iter()
                .map(|x| match x { &TypeAlias(ref v) => Some(v), _ => None })
                .flat_map(|op| op.into_iter())
                .collect::<Vec<&ast::TypeAlias>>();
        let type_declrs =
            self.top_levels.iter()
                .map(|x| match x { &TypeDeclr(ref v) => Some(v), _ => None })
                .flat_map(|op| op.into_iter())
                .collect::<Vec<&ast::TypeDeclr>>();
        (type_aliases, type_declrs)
    }
}
