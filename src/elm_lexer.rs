// A lexer to tokenize elm source code
// Currently, only aims to tokenize the export and import declarations.
//
// Shortly on the roadmap is documentation and top level declarations types.
//
// This probably will be mostly inspired by the haskell-compiler lexer
// <https://github.com/Marwes/haskell-compiler>
//
// Interner: I think this can be used in the parsing phase, tokenization
// is just a different view on a character streams, Internerization can be
// done at the parsing phase if we endup using too much memory.
//
// Location: it will eventually be necessary to handle column count for
// `let .. in` and `case` branches, however, don't implement something if
// it is not necessary *yet*.
// For the purposes of a pure parsing library with no end-user facing features
// it is not necessary to keep track of parsing failure locations, elm-make
// is probably the best compiler out there for telling people how to fix their
// code anyway.
use std::fmt;
use std::iter::Peekable;

///Tokens one expects to find in an elm source file
pub enum ElmToken {
    TopLevel, //Beginning of line for stuff that starts at column 0
    LParens,
    RParens,
    Comma,
    Operator(String),
    Ellision, //Export (..)
    Name(String), //an identifier
    Module,
    Exposing,
    Import,
    As,
    LBrace,
    RBrace,
    DocComment(String),
}
    //TODO: "effect", "where" for effet module declaration

impl fmt::Debug for ElmToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ElmToken::TopLevel => write!(f, "<^>"),
            &ElmToken::LParens => write!(f, "<(>"),
            &ElmToken::RParens => write!(f, "<)>"),
            &ElmToken::Comma => write!(f, "<,>"),
            &ElmToken::Operator(ref op) => write!(f, "<${}$>", op),
            &ElmToken::Ellision => write!(f, "<..>"),
            &ElmToken::Name(ref name) => write!(f, "<{}>", name),
            &ElmToken::Module => write!(f, "<module>"),
            &ElmToken::Exposing => write!(f, "<exposing>"),
            &ElmToken::Import => write!(f, "<import>"),
            &ElmToken::As => write!(f, "<as>"),
            &ElmToken::DocComment(_) => write!(f, "<{{-| ....... -}}>"),
            &ElmToken::LBrace => write!(f, "<{{>"),
            &ElmToken::RBrace => write!(f, "<}}>"),
        }
    }
}

/// is!(bind_name in sub1 | sub2 | sub3 | ... )
/// is true if bind_name matches one of the subN patterns
/// otherwise is false.
macro_rules! is {
    ( $bind_name:ident in $($sub_pat:pat)|+ ) =>
        (match $bind_name {$($sub_pat)|+ => true, _ => false})
}

macro_rules! to_str {
    ( $char_ident:ident ) => ( format!("{}", $char_ident) )
}

///Just returns the first character of a string
fn first_char(slice : &str) -> char {
    slice.chars().nth(0).unwrap()
}

fn is_operator(symbol : char) -> bool {
    is!(symbol in '+' | '-' | '*' | '/' | '.' | '!' | ':' | '=' | '<' | '>' | '|' | '&' )
}

///Tokenify a string to one single token.
///Be sure `text_token` is a valid token without padding whitespaces!
fn into_token(text_token : &str) -> ElmToken {
    match text_token {
        "module" => ElmToken::Module,
        "exposing" => ElmToken::Exposing,
        "import" => ElmToken::Import,
        "as" => ElmToken::As,
        ".." => ElmToken::Ellision,
        "," => ElmToken::Comma,
        "(" => ElmToken::LParens,
        ")" => ElmToken::RParens,
        "{" => ElmToken::LBrace,
        "}" => ElmToken::RBrace,

        op if is_operator(first_char(op)) =>
            ElmToken::Operator(String::from(op)),

        word if word.starts_with("{-|") && word.ends_with("-}") =>
            ElmToken::DocComment(String::from(
                    word.trim_left_matches("{-|").trim_right_matches("-}"))
            ),

        word if first_char(word).is_alphabetic() =>
            ElmToken::Name(String::from(word)),

        _ =>
            panic!("into_token didn't get a tokenizable item"),
    }
}


///Consumes the content of input that is necessary to build up the full
///name. Builds up a string into the input "into", untill "predicate"
///solves.
///It is similar to take_while, but it returns the last character.
fn consume_into<I>(
    input: &mut Peekable<I>,
    into: &mut String,
    predicate: fn(char) -> bool
) where I: Iterator<Item=char> {
    while let Some(&c) = input.peek() {
        if predicate(c) {
            input.next();
            into.push(c);
        } else {
            return;
        };
    };
}

fn consume_line_comment(input: &mut Iterator<Item=char>) {
    while let Some(c) = input.next() { if c == '\n' { return } };
}

fn consume_block_comment<I>(input: &mut Peekable<I>)
    -> Option<String>
    where I: Iterator<Item=char>
{
    let mut last = '-';
    let mut depth = 1i64;
    let mut accum = String::from("{-");
    let is_doc = {
        let second = input.next().unwrap();
        let third = *input.peek().unwrap();
        second == '-' && third == '|'
    };
    while let Some(c) = input.next() {
        if last == '{' && c == '-' {
            depth += 1
        } else if last == '-' && c == '}' {
            depth -= 1
        };
        if is_doc { accum.push(c) };
        last = c;
        if depth == 0 {
            return if is_doc { Some(accum) } else { None }
        };
    };
    None
}

fn consume_operator<I>(input: &mut Peekable<I>, into: &mut String)
    where I: Iterator<Item=char>
{
    consume_into(input, into, is_operator)
}
fn consume_name<I>(input: &mut Peekable<I>, into: &mut String)
    where I: Iterator<Item=char>
{
    consume_into(input, into, |c| c.is_alphanumeric() || c == '.')
}

///Spawns tokens while consuming the input stream.
///It is basically a GIANT (WOWOW!!) match case inside a
///while loop.
///The while loop is only useful in the case that the current
///character is useless, and we need to consume further to get
///a token we can return
pub fn next_token<I>(input : &mut Peekable<I>)
    -> Option<ElmToken>
    where I: Iterator<Item=char>
{
    while let Some(next_char) = input.next() {
        match next_char {
            // WHITESPACE
            '\r' => { continue },
            '\n' => {
                if input.peek().map_or(false, |&c| !c.is_whitespace()) {
                    return Some(ElmToken::TopLevel);
                } else {
                    continue;
                }
            },
            c if c.is_whitespace() => { continue },
            // DELIMITERS
            '(' => { return Some(ElmToken::LParens) },
            ')' => { return Some(ElmToken::RParens) },
            ',' => { return Some(ElmToken::Comma) },
            '{' => {
                if input.peek().map_or(false, |&c| c == '-') {
                    if let Some(doc) = consume_block_comment(input) {
                        return Some(into_token(doc.as_ref()));
                    } else {
                        continue;
                    }
                } else {
                    return Some(ElmToken::LBrace);
                };
            },
            '}' => { return Some(ElmToken::RBrace) },
            // OPERATORS
            c if is_operator(c) => {
                if c == '-' && input.peek().map_or(false, |&c| c == '-') {
                    consume_line_comment(input);
                    continue;
                }
                let mut into_buffer : String = to_str!(c);
                consume_operator(input, &mut into_buffer);
                return Some(into_token(into_buffer.as_ref()));
            },
            // NAMES
            c if c.is_alphabetic() => {
                let mut into_buffer : String = to_str!(c);
                consume_name(input, &mut into_buffer);
                return Some(into_token(into_buffer.as_ref()));
            },
            _ => { return None },
        }
    };
    return None;
}

pub struct IterLexer<Stream: Iterator<Item=char>> { input : Peekable<Stream> }
impl <Stream: Iterator<Item=char>> IterLexer<Stream> {
    pub fn new(input : Stream) -> IterLexer<Stream> {
        IterLexer { input : input.peekable(), }
    }
}

impl <Stream: Iterator<Item=char>> Iterator for IterLexer<Stream> {
    type Item = ElmToken;
    fn next(&mut self) -> Option<ElmToken> {
        next_token(&mut self.input)
    }
}
