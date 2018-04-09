//! A lazily evaluated parser
use std::iter::{SkipWhile,Peekable};

use lalrpop_util::ParseError as LalrpopError;
use itertools::{Coalesce,Itertools};

use super::grammar::{parse_ModuleDeclr,parse_Import, parse_TopDeclr};
use super::tree;
use super::filter_indent::TokenIterator;
use ::tokens::{ElmToken,LexError};
use ::lexer::{LexableIterator,Lexer};
use self::ElmToken::Newline;

quick_error! {
    #[derive(Debug)]
    pub enum ParserError {
        // Other(err: Box<::std::error::Error>) {
        //     description(err.description())
        //     from()
        //     cause(err)
        // }
        TokenAfterModuleDoc(token: ElmToken) {
            description("There is no new lines after a module declaration, \
            instead, the following token was found: {}")
        }
        EofAfterModuleDoc {
            description("The source file ends after the module documentation \
            comment")
        }
        LalrpopError(err: LalrpopError<u32,ElmToken,LexError>) {
            from()
        }
    }
}

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

enum ParserStage {
    ModuleDeclr,
    ModuleDoc,
    Imports,
    TopDeclrs,
    FullyParsed,
}


/// Holds the state of what was parsed up to now.
/// `stage` field is the progress we made in the parsing
/// of the file.
struct StreamParser<I: Iterator<Item=Loc<ElmToken>>> {
    input : OMG<I>,
    module_declr: Option<tree::ModuleDeclr>,
    module_doc: Option<String>,
    module_imports: Vec<tree::ElmImport>,
    top_levels: Vec<tree::TopDeclr>,
    stage: ParserStage,
}

impl<T: Iterator<Item=Loc<ElmToken>>> IntoParsed for T {}

/// Converts a token stream into a parser
trait IntoParsed: Iterator<Item=Loc<ElmToken>> {
    /// Add some processing to the input token stream and embed
    /// into the Iterator adaptator StreamParser at its initial
    /// stage.
    fn into_parsed(self) -> StreamParser<Self>
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
            module_declr: None,
            module_doc: None,
            module_imports: Vec::new(),
            top_levels: Vec::new(),
            stage: ParserStage::ModuleDeclr,
        }
    }
}

impl<I: Iterator<Item=Loc<ElmToken>>> StreamParser<I> {
    fn next_step(&mut self) -> Result<(),ParserError> {
        use self::ParserStage as PS;
        match self.stage {
            PS::ModuleDeclr => self.parse_module_declaration()?,
            PS::ModuleDoc => self.parse_module_doc()?,
            PS::Imports => self.parse_imports()?,
            PS::TopDeclrs => self.parse_declarations()?,
            PS::FullyParsed =>
                {/* TODO: decide if this should be an error */},
        };
        Ok(())
    }

    fn parse_module_declaration(&mut self) -> Result<(),ParserError> {
        let has_module_declaration = match self.input.peek() {
            Some(&(_, ElmToken::Name(ref name))) if name == "effect"  => true,
            Some(&(_, ElmToken::Port)) | Some(&(_, ElmToken::Module)) => true,
            _ => false,
        };
        self.stage = ParserStage::ModuleDoc;
        self.module_declr = if has_module_declaration {
               parse_ModuleDeclr(lalrpopify!(self.input, filter_nl))?
        } else { None };
        Ok(())
    }
    fn parse_module_doc(&mut self) -> Result<(),ParserError> {
        self.stage = ParserStage::Imports;
        self.module_doc = match self.input.peek() {
            Some(&(_, ElmToken::DocComment(_))) => {
                let module_doc = match self.input.next() {
                    Some((_,ElmToken::DocComment(content))) =>
                        Some(content),
                    anyelse =>
                        panic!("read a DocComment with a peek, \
                                now, I get something different!
                                : {:?}", anyelse),
                };
                // Consume newline after the doc comment (like a lalrpopify!
                // macro would do).
                match self.input.next() {
                    Some((_, ElmToken::Newline(_))) => module_doc,
                    None =>
                        return Err(ParserError::EofAfterModuleDoc),
                    Some(token) =>
                        return Err(ParserError::TokenAfterModuleDoc(token.1)),
                }
            },
            _ => None,
        };
        Ok(())
    }
    fn parse_imports(&mut self) -> Result<(),ParserError> {
        self.stage = ParserStage::TopDeclrs;
        while let Some(&(_, ElmToken::Import)) = self.input.peek() {
            let cur_import = parse_Import(lalrpopify!(self.input, filter_nl))?;
            // TODO: gracefully handle individually broken bits of code.
            self.module_imports.push(cur_import);
        }
        Ok(())
    }
    fn parse_declarations(&mut self) -> Result<(),ParserError> {
        self.stage = ParserStage::FullyParsed;
        loop {
            match parse_TopDeclr(lalrpopify!(self.input, filter_indent)) {
                Ok(val) => self.top_levels.push(val),
                Err(_) => break // TODO: handle this more gracefully
            };
        }
        Ok(())
    }

    /// Parse the file up to the given stage, or not at all if that stage
    /// was already reached.
    fn evaluate_up_to(&mut self, stage: ParserStage) -> Result<(),ParserError> {
        use self::ParserStage as SG;
        macro_rules! next_n {
            ($($count:tt ).+ ; $to_next:tt) => ({
                $(let _ignore = stringify!($count); $to_next.next_step()?;)+
                Ok(())
            })
        }
        match (stage, &self.stage) {
            (SG::ModuleDeclr, _) => Ok(()),
            (SG::ModuleDoc, &SG::ModuleDeclr) => next_n!(+;self),
            (SG::ModuleDoc, _) => Ok(()),
            (SG::Imports, &SG::ModuleDeclr) => next_n!(+.+;self),
            (SG::Imports, &SG::ModuleDoc) =>   next_n!(+  ;self),
            (SG::Imports, _) => Ok(()),
            (SG::TopDeclrs, &SG::ModuleDeclr) => next_n!(+.+.+;self),
            (SG::TopDeclrs, &SG::ModuleDoc) =>   next_n!(+.+  ;self),
            (SG::TopDeclrs, &SG::Imports) =>     next_n!(+    ;self),
            (SG::TopDeclrs, _) => Ok(()),
            (SG::FullyParsed, &SG::ModuleDeclr) => next_n!(+.+.+.+;self),
            (SG::FullyParsed, &SG::ModuleDoc) =>   next_n!(+.+.+  ;self),
            (SG::FullyParsed, &SG::Imports) =>     next_n!(+.+    ;self),
            (SG::FullyParsed, &SG::TopDeclrs) =>   next_n!(+      ;self),
            (SG::FullyParsed, _) => Ok(()),
        }
    }

    fn into_tree(mut self) -> Result<tree::ElmModule,ParserError> {
        self.evaluate_up_to(ParserStage::FullyParsed)?;
        let (name, exports) = match self.module_declr {
            Some(tree::ModuleDeclr {name, exports}) =>
                (Some(name), exports),
            None =>
                (None, tree::ExportList::Unqualified),
        };
        let doc = self.module_doc;
        let imports = self.module_imports;
        let tree::CoalecedTopDeclr {
            ports:list_ports, infixities, functions, types
        } = tree::into_tree(self.top_levels.into_iter());

        let ports = if list_ports.is_empty() { None } else { Some(list_ports) };
        Ok(tree::ElmModule {
            name, exports, doc, imports, types, functions, infixities, ports
        })
    }
}


/// A lazy parser.
///
/// It only consumes the characters needed to return the values requested
/// by the methods called on it.
///
/// The getter functions take a mutable borrow because it may need to
/// mutate in place the parser in order to evaluate (and remember) a
/// part of the parse tree.
pub struct Parser<I:Iterator<Item=char>> (StreamParser<Lexer<I>>);

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
        Parser(input.lex().into_parsed())
    }

    /// Returns the name of the module.
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
    /// let declaration = parser.module_declaration().unwrap();
    /// assert_eq!(declaration.name, "My.Great.Module".to_owned());
    /// ```
    pub fn module_declaration(&mut self) -> Option<&tree::ModuleDeclr> {
        self.0.evaluate_up_to(ParserStage::ModuleDoc).unwrap();
        self.0.module_declr.as_ref()
    }

    /// Returns the documentation string of the elm module, if it
    /// exists
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
        self.0.evaluate_up_to(ParserStage::Imports).unwrap();
        &self.0.module_doc
    }

    /// Returns the list of import declarations. Evaluating it if needed
    pub fn imports(&mut self) -> &[tree::ElmImport] {
        self.0.evaluate_up_to(ParserStage::TopDeclrs).unwrap();
        &self.0.module_imports
    }

    /// Parse the whole file and return the complete parse tree.
    pub fn into_parse_tree(self) -> tree::ElmModule {
        self.0.into_tree().unwrap()
    }
}

