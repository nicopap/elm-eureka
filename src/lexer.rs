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

///Tokenify a string to one single token.
///Be sure `text_token` is a valid token without padding whitespaces!
///Also, certain delimiters are not checked for.
///String and character literals are NOT handled, (it will panic)
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

        word if first_char(word).is_digit(10) =>
            ElmToken::Number(String::from(word)),

        _ =>
            panic!(format!("{{{}}} was not a recognizable token!", text_token)),
    }
}


/// Consumes the content of input that is necessary to build up the full
/// name. Builds up a string into the input "into", until "predicate"
/// solves.
///
/// NOTE: on the *consume* functions: they work without the
/// "opening" character. One needs to provide a `into` String
/// in which the first character is already written to have
/// the full token string.
fn consume_into<I>(
    input: &mut Peekable<I>,
    into: &mut String,
    predicate: fn(char) -> bool
)   where I: Iterator<Item=char>
{
    //let mut i = 0;
    while let Some(&c) = input.peek() {
        if predicate(c) {
            //i += (c == '\n') as i16;
            input.next();
            into.push(c);
        } else {
            return ;
        }
    }
}

/// Consumes the rest of a line until '\n' (does
/// not consume the end of line)
fn consume_line_comment<I>(input: &mut Peekable<I>)
    where I: Iterator<Item=char>
{
    while let Some(&c) = input.peek() {
        if c == '\n' { return }
        let _ = input.next();
    };
}

/// Consumes a block comment. Handles properly nested
/// block comments. This function doesn't accumulate
/// the input into a buffer. Instead, it returns a
/// string if the comment was a doc comment.
///
/// # Returns
/// `(Some(DocComment), #linesConsumed)`
fn consume_block_comment<I>(input: &mut Peekable<I>)
    -> (Option<String>, i16)
    where I: Iterator<Item=char>
{
    let mut last = '-';
    let mut depth = 1i64;
    let mut nl_consumed = 0;
    let mut accum = String::from("{-");
    let is_doc = {
        let second = input.next().unwrap();
        let third = *input.peek().unwrap();
        nl_consumed += (second == '\n') as i16;
        second == '-' && third == '|'
    };
    while let Some(c) = input.next() {
        nl_consumed += (c == '\n') as i16;
        if last == '{' && c == '-' {
            depth += 1
        } else if last == '-' && c == '}' {
            depth -= 1
        };
        if is_doc { accum.push(c) };
        last = c;
        if depth == 0 {
            return (if is_doc { Some(accum) } else { None }, nl_consumed)
        };
    };
    (None, nl_consumed)
}

fn consume_operator<I>(input: &mut Peekable<I>, into: &mut String)
    where I: Iterator<Item=char>
{
    consume_into(input, into, is_operator)
}

fn consume_name<I>(input: &mut Peekable<I>, into: &mut String)
    where I: Iterator<Item=char>
{
    consume_into(input, into, |c| c.is_alphanumeric() || c == '.' || c == '_')
}

/// Consumes a number literal. A number literal can be one of the
/// following:
/// ```
/// x[0-9a-fA-F]+
/// [0-9]*(.[0-9]+)?(e[-+]?[0-9]+)?
/// ```
/// Currently, also accepts [0-9]x[0-9a-fA-F]+
fn consume_number<I>(input: &mut Peekable<I>, into: &mut String)
    where I: Iterator<Item=char>
{
    let is_hexa = match input.peek() { Some(&'x') => true, _ => false };
    if is_hexa {
        into.push(input.next().unwrap());
        consume_into(input, into, |c| c.is_digit(16))
    } else {
        let mut radix = false;
        let mut has_digit = false;
        let mut just_radix = false;
        while let Some(&c) = input.peek() {
            match c {
                val if val.is_digit(10) => { radix = just_radix; },
                '.' if !has_digit => { has_digit = true; },
                'e' if !just_radix => { just_radix = true; has_digit = true},
                '+' | '-' if just_radix && !radix => { radix = just_radix },
                _ => { return; }
            }
            input.next();
            into.push(c);
        }
    };
}

/// Why use a string to represent the content of a char? well, escape
/// sequences, my dear!
/// This just makes sure that '\'' doesn't trip up the lexer
/// We intend to parse valid code anyway, we can accept garbage to an
/// extent.
/// NOTE: doesn't include the closing ' in the `into` String.
fn consume_char(input: &mut Iterator<Item=char>, into: &mut String) {
    let mut prev_is_escape = false;
    while let Some(c) = input.next() {
        if c == '\'' && !prev_is_escape {
            return;
        }
        prev_is_escape = c == '\\' && !prev_is_escape;
        into.push(c);
    };
}

/// This is the same parser as char, just ignores \"
/// NOTE: doesn't include the closing " in the `into` String.
fn consume_string(input: &mut Iterator<Item=char>, into: &mut String) -> i16 {
    let mut prev_is_escape = false;
    let mut nl_consumed = 0;
    while let Some(c) = input.next() {
        nl_consumed += (c == '\n') as i16;
        if c == '"' && !prev_is_escape {
            return nl_consumed;
        }
        prev_is_escape = c == '\\' && !prev_is_escape;
        into.push(c)
    }
    nl_consumed
}

/// Consumes up to the first non-whitespace. returns the proper
/// newline token If there is extra newlines before the next
/// token, the Token is created accordingly.
///
/// # Returns
/// `(Some(parsedToken), #LinesConsumed)`
fn consume_newline<I>(input: &mut Peekable<I>, line: i16)
    -> (Option<ElmToken>, i16)
    where I: Iterator<Item=char>
{
    let mut nl_consumed = 0;
    let mut current_column = 0;
    while let Some(&c) = input.peek() {
        match c {
            ' ' => {current_column += 1},
            '\n' => {nl_consumed += 1; current_column = 0},
            _ => {
                let this_line = nl_consumed + line;
                return (
                     Some( ElmToken::Newline(this_line, current_column) ),
                     nl_consumed
                )
            },
        };
        input.next();
    }
    (None, nl_consumed)
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
    fn next(&mut self) -> Option<ElmToken> {
        let input = &mut self.input;
        while let Some(next_char) = input.next() {
            match next_char {
                '\r' => { continue },
                '\n' => {
                    let (token, nl_consumed) = consume_newline(input, self.line_loc + 1);
                    self.line_loc += nl_consumed + 1;
                    return token;
                },
                c if c.is_whitespace() => { continue },
                '[' => { return Some(ElmToken::LBracket) },
                ']' => { return Some(ElmToken::RBracket) },
                '(' => { return Some(ElmToken::LParens) },
                ')' => { return Some(ElmToken::RParens) },
                ',' => { return Some(ElmToken::Comma) },
                '}' => { return Some(ElmToken::RBrace) },
                '{' => {
                    if input.peek().map_or(false, |&c| c == '-') {
                        match consume_block_comment(input) {
                            (Some(doc), nl_consumed) => {
                                self.line_loc += nl_consumed;
                                return Some(into_token(doc.as_ref()));
                            },
                            (None, nl_consumed) => {
                                self.line_loc += nl_consumed;
                                continue;
                            }
                        }
                    } else {
                        return Some(ElmToken::LBrace);
                    };
                },
                c if is_operator(c) => {
                    if c == '-' && input.peek().map_or(false, |&c| c == '-') {
                        consume_line_comment(input);
                        continue;
                    }
                    let mut into_buffer : String = to_str!(c);
                    consume_operator(input, &mut into_buffer);
                    return Some(into_token(into_buffer.as_ref()));
                },
                c if c.is_alphabetic() => {
                    let mut into_buffer : String = to_str!(c);
                    consume_name(input, &mut into_buffer);
                    return Some(into_token(into_buffer.as_ref()));
                },
                '_' => { return Some(ElmToken::Underscore) },
                '"' => {
                    let mut into_buffer = String::new();
                    let nl_consumed = consume_string(input, &mut into_buffer);
                    self.line_loc += nl_consumed;
                    return Some(ElmToken::StringLit(into_buffer));
                },
                '\'' => {
                    let mut into_buffer = String::new();
                    consume_char(input, &mut into_buffer);
                    return Some(ElmToken::Char(into_buffer));
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

#[cfg(test)] mod tests {
    use super::*;

    macro_rules! s {
        ($fn_to_call:ident, $str_to_conv:expr) => (
            $fn_to_call(String::from($str_to_conv))
        )
    }
    const UNORDERED_LIST_EXAMPLE : &str = r#"import Html exposing (li, text, ul)
import Html.Attributes exposing (class)


{-| This {- Hello :)-}

Et maintenant le voyage au supermarché!
-}
main =
  ul [class "grocery-list"]
    [ li [] [text "Pamplemousse"]
    , li [] [text "Ananas"]
    , li [] [text "Jus d'orange"]
    , li [] [text "Bœuf"]
    , li [] [text "Soupe du jour"]
    , li [] [text "Camembert"]
    , li [] [text "Jacques Cousteau"]
    , li [] [text "Baguette"]
    ]


-- Thanks to "Flight of the Conchords" for creating "Foux Du Fafa"
"#;

    #[test] fn test_lexer() {
        use tokens::ElmToken::*;

        let str_input = String::from(UNORDERED_LIST_EXAMPLE);
        let char_input = str_input.chars();
        let lexer = Lexer::new(char_input);
        let token_vec = lexer.collect::<Vec<_>>();
        assert_eq!(
            token_vec, vec![Import, s!(Name,"Html"), Exposing, LParens,
            s!(Name,"li"), Comma, s!(Name,"text"), Comma, s!(Name,"ul"),
            RParens, Newline(2, 0), Import, s!(Name,"Html.Attributes"),
            Exposing, LParens, s!(Name,"class"), RParens, Newline(5,0),
            DocComment(String::from(r#" This {- Hello :)-}

Et maintenant le voyage au supermarché!
"#)), Newline(9,0), s!(Name,"main"), Assign,
            Newline(10,2), s!(Name,"ul"), LBracket, s!(Name,"class"), s!(StringLit,"grocery-list"), RBracket,
            Newline(11,4),LBracket,s!(Name,"li"),LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Pamplemousse"), RBracket,
            Newline(12,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Ananas"), RBracket,
            Newline(13,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Jus d'orange"), RBracket,
            Newline(14,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Bœuf"), RBracket,
            Newline(15,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Soupe du jour"), RBracket,
            Newline(16,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Camembert"), RBracket,
            Newline(17,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Jacques Cousteau"), RBracket,
            Newline(18,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Baguette"), RBracket,
            Newline(19,4), RBracket, Newline(22,0)]);
    }
    macro_rules! consumer_test {
        ($input_value:expr, $buffer_res:expr, $input_remain:expr, $to_test:ident) => (
            let mut buffer = String::new();
            let mut input = $input_value.chars().peekable();
            buffer.push(input.next().unwrap());
            $to_test(&mut input, &mut buffer);
            assert_eq!(buffer, $buffer_res);
            assert_eq!(input.collect::<String>(), $input_remain);
        )
    }
    #[test] fn test_consume_operator() {
        consumer_test!( "+*--12", "+*--", "12", consume_operator);
        consumer_test!( "<*| wow |*>", "<*|", " wow |*>", consume_operator);
    }
    #[test] fn test_consume_char() {
        consumer_test!( r#"'\xfff' 10"#, r#"'\xfff"#, " 10", consume_char);
        consumer_test!( r#"'\\' ten"#, r#"'\\"#, " ten", consume_char);
        consumer_test!( r#"'\''''"#, r#"'\'"#, "''", consume_char);
    }
    #[test] fn test_consume_number() {
        consumer_test!( "0xabcd-10", "0xabcd", "-10", consume_number);
        consumer_test!( "10e+34-10", "10e+34", "-10", consume_number);
        consumer_test!( "3.1415-10", "3.1415", "-10", consume_number);
    }
    #[test] fn test_consume_name() {
        consumer_test!( "Some.test-10", "Some.test", "-10", consume_name);
    }
    macro_rules! block_test_boilerplate {
        ($input_value:expr, $buffer_res:expr, $input_remain:expr) => (
            let mut input = $input_value.chars().peekable();
            let (content, _) = consume_block_comment(&mut input);
            assert_eq!(content, $buffer_res);
            assert_eq!(input.collect::<String>(), $input_remain);
        )
    }
    #[test] fn test_consume_block_comment1() {
        block_test_boilerplate!("-hello world!!-}float", None, "float");
    }
    #[test] fn test_consume_block_comment2() {
        block_test_boilerplate!(
            "-hello {-world!!-}float-}remains",
            None,
            "remains"
        );
    }
    #[test] fn test_consume_block_comment3() {
        block_test_boilerplate!(
            "-|hello {- hi-} world!!-}float",
            Some(String::from("{-|hello {- hi-} world!!-}")),
            "float"
        );
    }
}
