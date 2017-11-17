//! A lexer to tokenize elm source code
//!
//! Currently, tokenizes most valid elm code, but also
//! a lot of invalid elm code. One soar point is the
//! handling of literals.
//!
//! Currently multiline string literals are not
//! lexed correctly (though this doesn't impeed
//! to the parsing of the rest of the file, as
//! the content of the string will be captured
//! into a string literal token)
//!
//! Indentation at newline is kept track of with the
//! Newline token. It should eventually contain the
//! line number. Currently, the line field is always
//! 1, the column field is set correctly.
//!
//! In term of efficiency: this is a bit difficult to
//! parallelize (ie: divide the file in N bits to tokenize
//! and then concatenate the result) because of block
//! comments and string literals. Otherwise the lexer
//! is position independent.
//!
//! Shortly on the roadmap is correct line counting.
//!
//! Note that the shortcomming of this library, while
//! it would be terrible for a compiler, are not too
//! problematic: we mostly intend to parse *valid* elm
//! code, so this is an expectation we kinda can deal
//! with.
//! Plus, we can parallelize by lexing multiple files
//! at the same time. Since we already lex a 5K file
//! in about 10ms, I don't think the lattency here
//! is much of a problem.

use std::iter::Peekable;
use tokens::ElmToken;

macro_rules! to_str {
    ( $char_ident:ident ) => ( format!("{}", $char_ident) )
}

///Just returns the first character of a string
fn first_char(slice : &str) -> char {
    slice.chars().nth(0).unwrap()
}

fn is_operator(symbol : char) -> bool {
    match symbol {
        '+' | '-' | '*' | '/' | '.' |
        '!' | ':' | '=' | '<' | '>' |
        '|' | '&' | '%' | '^' | '#' |
        '$' | '?' | '@' | '~' | '\\' => true,
        _ => false
    }
}

fn is_number(symbol : char) -> bool {
    match symbol {
        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'x' |
        '.' | '0' | '1' | '2' | '3' | '4' | '5' |
        '6' | '7' | '8' | '9' | '-' => true,
        _ => false
    }
}

///Tokenify a string to one single token.
///Be sure `text_token` is a valid token without padding whitespaces!
///Also, certain delimiters are not checked for.
fn into_token(text_token : &str) -> ElmToken {
    match text_token {
        "module" => ElmToken::Module,
        "exposing" => ElmToken::Exposing,
        "import" => ElmToken::Import,
        "as" => ElmToken::As,
        ".." => ElmToken::Ellision,
        "{" => ElmToken::LBrace,
        "λ" => ElmToken::Lambda,
        "\\" => ElmToken::Lambda,
        "->" => ElmToken::RArrow,
        "case" => ElmToken::Case,
        "of" => ElmToken::Of,
        "if" => ElmToken::If,
        "then" => ElmToken::Then,
        "else" => ElmToken::Else,
        "|" => ElmToken::Pipe,
        "=" => ElmToken::Assign,
        ":" => ElmToken::TypeDeclr,
        "type" => ElmToken::Type,
        "alias" => ElmToken::Alias,
        "infixr" => ElmToken::Infixr,
        "infixl" => ElmToken::Infixl,
        "port" => ElmToken::Port,
        "effect" => ElmToken::Effect,
        "where" => ElmToken::Where,
        "let" => ElmToken::Let,
        "in" => ElmToken::In,

        op if is_operator(first_char(op)) =>
            ElmToken::Operator(String::from(op)),

        word if word.starts_with("{-|") && word.ends_with("-}") =>
            ElmToken::DocComment(String::from(
                    word.trim_left_matches("{-|").trim_right_matches("-}")
            )),

        word if first_char(word).is_alphabetic() =>
            ElmToken::Name(String::from(word)),

        word if first_char(word).is_numeric() =>
            ElmToken::Number(String::from(word)),

        word if first_char(word) == '"' =>
            ElmToken::StringLit(String::from(
                    word.trim_left_matches("\"").trim_right_matches("\"")
            )),

        word if first_char(word) == '\'' =>
            ElmToken::Char(String::from(
                    word.trim_left_matches("'").trim_right_matches("'")
            )),
        _ =>
            panic!(format!("{{{}}} was not a recognizable token!", text_token)),
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

///This is a mess. There isn't really indications on the elm
///site on what number syntax is supported, I'll just pretend
///number with thrown in abcdefx. is fine. 1-xxxefbae3.2.-..2--.. <- valid
fn consume_number<I>(input: &mut Peekable<I>, into: &mut String)
    where I: Iterator<Item=char>
{
    consume_into( input, into, is_number)
}

///Why use a string to represent the content of a char? well, escape
///sequences, my dear!
///This just makes sure that '\'' doesn't trip up the lexer
///We intend to parse valid code anyway, we can accept garbage to an
///extent.
fn consume_char(input: &mut Iterator<Item=char>, into: &mut String) {
    let mut prev_is_escape = false;
    while let Some(c) = input.next() {
        into.push(c);
        if c == '\'' && !prev_is_escape {
            return
        } else {
            prev_is_escape = c == '\\' && !prev_is_escape
        };
    };
}

///This is the same parser as char, just ignores \"
fn consume_string(input: &mut Iterator<Item=char>, into: &mut String) {
    let mut prev_is_escape = false;
    while let Some(c) = input.next() {
        into.push(c);
        if c == '"' && !prev_is_escape {
            return
        };
        prev_is_escape = c == '\\' && !prev_is_escape;
    };
}

///Consumes up to the first non-whitespace. returns the proper
///newline token If there is extra newlines before the next
///token, the Token is created accordingly.
fn consume_newline<I>(input: &mut Peekable<I>, line: i16)
    -> Option<ElmToken>
    where I: Iterator<Item=char>
{
    let mut current_line = line;
    let mut current_column = 0i16;
    while let Some(&c) = input.peek() {
        if c == ' ' {
            current_column += 1;
        } else if c == '\n' {
            current_line += 1;
            current_column = 0;
        } else {
            return Some(ElmToken::Newline(current_line, current_column));
        }
        input.next();
    };
    None
}


/// An elm lexer.
///
/// The lexer is a statefull adaptator that transforms
/// a stream of characters into a stream of elm tokens.
/// A token is a convinient data structures for parsing
/// and building an abstract syntax tree.
///
/// The lexer is an iterator, and does everything as
/// tokens are requested from it. This is to offer
/// the user most freedom about how they consume
/// streams.
///
/// You could collect into a vector all the tokens
/// before processing, or you could defer up to the
/// last moment the evaluation of tokens.
///
/// # Current Limitations
///
/// The line location is not well kept track of,
/// so any Newline token will have its line value
/// set at `1`.
///
/// Strings finishing by a `\"` seems to be
/// erroneously lexed.
pub struct Lexer<Stream: Iterator<Item=char>> {
    input : Peekable<Stream>,
    line_loc : i16,
}

impl <Stream: Iterator<Item=char>> Lexer<Stream> {
    /// Creates a new lexer wrapping the
    /// character stream given as input.
    pub fn new(input : Stream) -> Lexer<Stream> {
        Lexer {
            input : input.peekable(),
            line_loc : 1,
        }
    }
}

impl <Stream: Iterator<Item=char>> Iterator for Lexer<Stream> {
    type Item = ElmToken;

    /// Spews tokens while consuming the input stream.
    /// Behaves nicely on a file without syntax errors.
    ///
    /// # Panics
    /// The iterator may panic in two circumstances:
    ///
    /// * Somehow, the lexer tried to turn into a
    ///   token something that it shouldn't
    ///
    /// * There is a block comment that never closes
    ///
    /// # Errors
    /// The stream may terminate early for
    /// undeterminate reasons, mostly broken code
    fn next(&mut self) -> Option<ElmToken>
    {
        let input = &mut self.input;
        while let Some(next_char) = input.next() {
            match next_char {
                // WHITESPACE
                '\r' => { continue },
                '\n' => { return consume_newline(input, 0) },
                c if c.is_whitespace() => { continue },
                // DELIMITERS
                '[' => { return Some(ElmToken::LBracket) },
                ']' => { return Some(ElmToken::RBracket) },
                '(' => { return Some(ElmToken::LParens) },
                ')' => { return Some(ElmToken::RParens) },
                ',' => { return Some(ElmToken::Comma) },
                '}' => { return Some(ElmToken::RBrace) },
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
                '_' => { return Some(ElmToken::Underscore) },
                //Literals
                '"' => {
                    let mut into_buffer = String::from("\"");
                    consume_string(input, &mut into_buffer);
                    return Some(into_token(into_buffer.as_ref()));
                },
                '\'' => {
                    let mut into_buffer = String::from("'");
                    consume_char(input, &mut into_buffer);
                    return Some(into_token(into_buffer.as_ref()));
                },
                c @ '0' ... '9' => {
                    let mut into_buffer : String = to_str!(c);
                    consume_number(input, &mut into_buffer);
                    return Some(into_token(into_buffer.as_ref()));
                },
                c => { println!("######{}#####", c); return None },
            }
        };
        return None;
    }
}
