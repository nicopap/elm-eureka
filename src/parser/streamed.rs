//! A lazily evaluated parser
use std::iter::{Peekable,SkipWhile};
use std::fmt;
use std::mem::replace;

use itertools::{Coalesce,Itertools};

use super::grammar::{parse_ModuleDeclr,parse_Import, parse_TopDeclr};
use super::tree;
use ::tokens::ElmToken;
use ::lexer::{LexableIterator,Lexer};
use self::ElmToken::{DocComment, Newline, Name, LParens, Port};

macro_rules! not_match {
    ($to_match:pat) => ( |x| match *x { $to_match => false, _ => true })
}
macro_rules! is_match {
    ($to_match:pat) => ( |x| match x { $to_match => true, _ => false })
}

// type SpanToken = Result<(usize, ElmToken, usize), LexError>;

// wrappers to deal with lazy input stream reification.
type CoalSig<X> = fn(X, X)->Result<X, (X,X)>;
type OMG<I>=Peekable<Coalesce<
    SkipWhile< I,for<'r> fn(&'r ElmToken)->bool >,
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
        let is_newline : fn(&ElmToken) -> bool = is_match!(&Newline(_,_));
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
                self.skip_while(is_newline)
                    .coalesce(remove_dup_newlines)
                    .peekable(),
            stage : StageModuleDeclr,
        }
    }
}
impl<T: Iterator<Item=ElmToken>> IntoParsed for T {}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageModuleDeclr> {
    fn next_step(mut self) -> StreamParser<I,StageModuleDoc> {
        // Consumes the module declaration, also consumes the next newline.
        let declr_stream =
            self.input
                .by_ref()
                .take_while(not_match!(Newline(_,0)))
                .filter(not_match!(Newline(_,_)))
                .enumerate()
                .map(|(i, token)| Ok((i, token, i+1)))
                .collect::<Vec<_>>();
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
        let mut module_imports : Vec<tree::ElmImport> = Vec::new();
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
                    top_levels.push(
                        parse_TopDeclr(preparsed_tokens)
                            .expect("Function Parsing Error")
                    );
                },
                Some(_) => {
                    let toplevel_stream =
                        self.input
                            .by_ref()
                            .take_while(not_match!(ElmToken::Newline(_,0)))
                            .filter(not_match!(ElmToken::Newline(_,_)))
                            .enumerate()
                            .map(|(i, token)| Ok((i, token, i+1)));
                    top_levels.push(
                        parse_TopDeclr(toplevel_stream)
                            .expect("Parsing Error")
                    );
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

fn into_tree(parser: StageEof) -> tree::ElmModule {
    let StageEof{module_declr,module_doc,module_imports,top_levels}=parser;
    let tree::ModuleDeclr {name, exports} = module_declr;
    let doc = module_doc;
    let imports = module_imports;
    let (list_ports, infixities, functions, types) =
        tree::into_tree(top_levels.into_iter());
    let ports = if list_ports.is_empty() { None } else { Some(list_ports) };
    tree::ElmModule {
        name,
        exports,
        doc,
        imports,
        types,
        functions,
        infixities,
        ports
    }
}

enum ParserState<I:Iterator<Item=ElmToken>> {
    StageModuleDeclr(StreamParser<I,StageModuleDeclr>),
    StageModuleDoc(StreamParser<I,StageModuleDoc>),
    StageImports(StreamParser<I,StageImports>),
    StageTopDeclrs(StreamParser<I,StageTopDeclrs>),
    StageFullyParsed(tree::ElmModule),
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
/// The `get_*` functions take a mutable borrow because it may need to
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
    pub fn new(input: I) -> Parser<I> {
        Parser(ParserState::StageModuleDeclr(input.lex().into_parsed()))
    }

    /// Parse the file up to the given stage.
    fn evaluate_up_to(&mut self, stage: ParserStage) {
        use self::ParserState as PS;
        use self::ParserStage as PT;
        let old_stage = replace(&mut self.0, PS::Nothing);
        match (stage, old_stage) {
            (PT::ModuleDoc, PS::StageModuleDeclr(parser)) =>{
                self.0 = PS::StageModuleDoc(parser.next_step());
            },
            (PT::ModuleDoc, anyelse) =>{ self.0 = anyelse; },

            (PT::Imports, PS::StageModuleDeclr(parser)) =>{
                self.0 = PS::StageImports(
                    parser.next_step().next_step()
                );
            },
            (PT::Imports, PS::StageModuleDoc(parser)) =>{
                self.0 = PS::StageImports(
                    parser.next_step()
                );
            },
            (PT::Imports, anyelse) =>{ self.0 = anyelse; },

            (PT::TopDeclrs, PS::StageModuleDeclr(parser)) =>{
                self.0 = PS::StageTopDeclrs(
                    parser.next_step().next_step().next_step()
                );
            },
            (PT::TopDeclrs, PS::StageModuleDoc(parser)) =>{
                self.0 = PS::StageTopDeclrs(
                    parser.next_step().next_step()
                );
            },
            (PT::TopDeclrs, PS::StageImports(parser)) =>{
                self.0 = PS::StageTopDeclrs(
                    parser.next_step()
                );
            },
            (PT::TopDeclrs, anyelse) =>{ self.0 = anyelse; },

            (PT::FullyParsed, PS::StageModuleDeclr(parser)) =>{
                self.0 = PS::StageFullyParsed(into_tree(
                    parser.next_step().next_step().next_step().next_step().stage
                ));
            },
            (PT::FullyParsed, PS::StageModuleDoc(parser)) =>{
                self.0 = PS::StageFullyParsed(into_tree(
                    parser.next_step().next_step().next_step().stage
                ));
            },
            (PT::FullyParsed, PS::StageImports(parser)) =>{
                self.0 = PS::StageFullyParsed(into_tree(
                    parser.next_step().next_step().stage
                ));
            },
            (PT::FullyParsed, PS::StageTopDeclrs(parser)) =>{
                self.0 = PS::StageFullyParsed(into_tree(
                    parser.next_step().stage
                ));
            },
            (PT::FullyParsed, anyelse) =>{ self.0 = anyelse; },
        }
    }

    /// Returns the export list of the module. Evaluating it if needed
    pub fn get_module_exports<'a>(&'a mut self) -> &'a tree::ExportList {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        match self.0 {
            PS::StageModuleDoc(ref parser) => &parser.stage.module_declr.exports,
            PS::StageImports(ref parser) => &parser.stage.module_declr.exports,
            PS::StageTopDeclrs(ref parser) => &parser.stage.module_declr.exports,
            PS::StageFullyParsed(ref module_tree) => &module_tree.exports,
            _ => unreachable!()
        }
    }

    /// Returns the name of the module. Evaluating it if needed
    pub fn get_module_name<'a>(&'a mut self) -> &'a String {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::ModuleDoc);
        match self.0 {
            PS::StageModuleDoc(ref parser) => &parser.stage.module_declr.name,
            PS::StageImports(ref parser) => &parser.stage.module_declr.name,
            PS::StageTopDeclrs(ref parser) => &parser.stage.module_declr.name,
            PS::StageFullyParsed(ref module_tree) => &module_tree.name,
            _ => unreachable!()
        }
    }

    /// Returns the export list of the module. Evaluating it if
    /// needed
    pub fn get_module_doc<'a>(&'a mut self) -> &'a Option<String> {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::Imports);
        match self.0 {
            PS::StageImports(ref parser) => &parser.stage.module_doc,
            PS::StageTopDeclrs(ref parser) => &parser.stage.module_doc,
            PS::StageFullyParsed(ref module_tree) => &module_tree.doc,
            _ => unreachable!()
        }
    }

    /// Returns the list of import declarations. Evaluating it if needed
    pub fn get_imports<'a>(&'a mut self) -> &'a [tree::ElmImport] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::TopDeclrs);
        match self.0 {
            PS::StageTopDeclrs(ref parser) => &parser.stage.module_imports,
            PS::StageFullyParsed(ref module_tree) => &module_tree.imports,
            _ => unreachable!()
        }
    }

    /// Returns the list of function declarations. The
    /// whole file is parsed in order to to generate that list
    pub fn get_functions<'a>(&'a mut self) -> &'a [tree::FunctionDeclaration] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::StageFullyParsed(ref module_tree) => &module_tree.functions,
            _ => unreachable!()
        }
    }

    /// Returns the list of type declarations. The
    /// whole file is parsed in order to to generate that list
    pub fn get_types<'a>(&'a mut self) -> &'a [tree::TypeDeclaration] {
        use self::ParserState as PS;
        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::StageFullyParsed(ref module_tree) => &module_tree.types,
            _ => unreachable!()
        }
    }

    /// Parse the whole file and return the complete parse tree.
    pub fn into_parse_tree(mut self) -> tree::ElmModule {
        use self::ParserState as PS;

        self.evaluate_up_to(ParserStage::FullyParsed);
        match self.0 {
            PS::StageFullyParsed(module_tree) => module_tree,
            _ => unreachable!()
        }
    }
}

impl<I:Iterator<Item=char>> fmt::Debug for Parser<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParserState as PS;
        match self.0 {
            PS::StageFullyParsed(ref complete_parse_tree) => {
                write!(f, "{:#1?}", complete_parse_tree)
            },
            _ => panic!("not yet implemented, silly boy"),
        }
    }
}
