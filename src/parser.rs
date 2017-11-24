//! An efficient implementation of an elm parser

use std::iter::{Peekable,SkipWhile};

use itertools::{Coalesce,Itertools};
use either::Either;
use either::Either::{Left,Right};

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
                (Newline(_,0), Newline(x,0)) => Ok(Newline(x,0)),
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
            let import_stream : Vec<SpanToken> =
                self.input
                    .by_ref()
                    .take_while(not_match!(Newline(_,0)))
                    .filter(not_match!(Newline(_,_)))
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

// Detects the significant indentation markers in a list of
// tokens, filter out the meaningless ones.
fn filter_indent(tokens : Vec<ElmToken>) -> Vec<ElmToken> {
    #[derive(PartialEq, Debug, Copy, Clone)] enum IndentSource { Let, Of }
    let mut indent_stack : Vec<(IndentSource,i16)> = Vec::new();
    let mut ret : Vec<ElmToken> = Vec::new();
    let mut indenter : Option<IndentSource> = None;

    // Finds the corresponding indent in vec. If there is no corresponding
    // indent, returns None.
    // There is no corresponding indent if:
    // - No matching indent level exists in vec, such as all succeeding
    //   `IndentSource` are `IndentSource::Of`.
    fn identify_ident(
        stack_index : usize,
        to_find : i16,
        vec : &mut Vec<(IndentSource,i16)>
        ) -> Option<usize>
    {
        let (src, indent) = vec[stack_index];
        if indent == to_find {
            Some(stack_index)
        } else if stack_index == 0 {
            None
        } else if src == IndentSource::Of {
            identify_ident(stack_index - 1, to_find, vec)
        } else {
            None
        }
    }
    for token in tokens {
        match token {
            // TODO: keep track of location after the `let` keyword if
            // the next Token doesn't start at a new line
            ElmToken::Let => {
                ret.push(ElmToken::Let);
                indenter = Some(IndentSource::Let);
            },
            ElmToken::In => {
                ret.push(ElmToken::In);
                // We didn't start a newline after `let` keyword
                // OTHERWISE
                // Remove all indents up to the corresponding `let`
                // keyword.
                if indenter == Some(IndentSource::Let) {
                    indenter = None
                } else { while
                    match indent_stack.pop() {
                        Some((IndentSource::Let,_)) => false,
                        _ => true,
                    }{
                }}
            },
            ElmToken::Newline(line,column) => {
                match indenter {
                    Some(source) => {
                        indent_stack.push((source,column));
                        ret.push(Newline(line, column));
                        indenter = None;
                    },
                    None if !indent_stack.is_empty() => {
                        identify_ident(
                            indent_stack.len()-1,
                            column,
                            &mut indent_stack
                        ).map(|idx| {
                            indent_stack.truncate(idx+1);
                            ret.push(Newline(line, column));
                        });
                    },
                    None => {},
                }
            },
            ElmToken::Of => {
                ret.push(ElmToken::Of);
                indenter = Some(IndentSource::Of);
            },
            any_other => {
                ret.push(any_other);
            },
        }
    }
    ret
}

impl<I:Iterator<Item=ElmToken>> StreamParser<I,StageTopDeclrs> {
    fn next_step(mut self) -> StreamParser<I,StageEof> {
        // TODO: this is flawed because it doesn't capture lazily the
        // tokens for a type declaration.
        // TODO: Needs function and function type parsing.
        let mut top_levels = Vec::new();
        loop {
            match self.input.peek() {
                Some(&Name(_)) | Some(&LParens) | Some(&Port) => {
                    let unparsed_tokens : Vec<ElmToken> =
                        self.input
                            .by_ref()
                            .take_while(not_match!(ElmToken::Newline(_,0)))
                            .collect();
                    let preparsed_tokens =
                        filter_indent(unparsed_tokens)
                            .into_iter()
                            .enumerate()
                            .map(|(i, token)| Ok((i, token, i+1)))
                            .collect();
                    top_levels.push(Left(preparsed_tokens));
                },
                Some(_) => {
                    let toplevel_stream : Vec<SpanToken> =
                        self.input
                            .by_ref()
                            .take_while(not_match!(ElmToken::Newline(_,0)))
                            .filter(not_match!(ElmToken::Newline(_,_)))
                            .enumerate()
                            .map(|(i, token)| Ok((i, token, i+1)))
                            .collect();
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
///
/// * Better API, including:
///   * association between symbols in various location of the AST
///
/// Maybe this role is better filed with another data structure,
/// which would be exposed to the outside world?
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
