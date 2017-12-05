//! A lexer to tokenize elm source code
//!
//! Currently, tokenizes most valid elm code, but also
//! a lot of invalid elm code. One sore point is the
//! handling of literals.
//!
//! Currently multiline string literals are not
//! lexed correctly (though this doesn't impeed
//! to the parsing of the rest of the file, as
//! the content of the string will be captured
//! into a string literal token)
//!

use std::error;
use std::iter::Peekable;

use tokens::ElmToken;

macro_rules! to_str {
    ( $char_ident:ident ) => ( format!("{}", $char_ident) )
}

macro_rules! first_char {
    ($string:ident) => (
        $string.chars().nth(0).unwrap()
    )
}

macro_rules! ret {
    ($retval:expr) => (
        return Some($retval)
    )
}

#[derive(Debug,PartialEq,Clone)]
pub enum LexError {
    Char(&'static str),
    StringLit(&'static str),
    Number(&'static str),
    Generic,
    Foreign,//(Box<error::Error>),
    BlockComment,
}

pub struct Lexed( Result< ((u16,u16),ElmToken,(u16,u16)), LexError >);

impl Lexed {
    pub fn map<F>(self, closure: F) -> Lexed
        where F: FnOnce(ElmToken) -> ElmToken
    {
        Lexed( match self.0 {
            Ok((begin, token, end)) => Ok((begin, closure(token), end)),
            Err(x) => Err(x),
        })
    }

    pub fn and_then<E,F>(self, closure: F) -> Lexed
        where F: FnOnce(ElmToken) -> Result<ElmToken,Box<E>>,
              E: error::Error,
    {
        Lexed(match self.0 {
            Ok((begin, token, end)) =>
                closure(token)
                    .map(|x|(begin,x,end))
                    .map_err(|_| LexError::Foreign),
            Err(x) => Err(x),
        })
    }

    pub fn token(&self) -> Option<&ElmToken> {
        match self.0 {
            Ok((_,ref token,_)) => Some(token),
            Err(_) => None,
        }
    }
}

fn is_operator(symbol : char) -> bool {
    match symbol {
        '+' | '-' | '*' | '/' | '.' | 'λ' |
        '!' | ':' | '=' | '<' | '>' |
        '|' | '&' | '%' | '^' | '#' |
        '$' | '?' | '@' | '~' | '\\' => true,
        _ => false
    }
}

/// Converts a name/operator string into an elm token.
/// It checks for keywords and the type of name it is, and
/// returns the token accordingly.
///
/// The input `text_token` must be a full word, such as a name or
/// operator.
///
/// # Examples
///
/// ```ignore
/// let token1 = String::from("then");
/// assert_eq!(into_keyword(token1), ElmToken::Then);
///
/// let token2 = String::from("effect");
/// assert_eq!(into_keyword(token2), ElmToken::Name(String::from("effect")));
///
/// let token3 = String::from("<*|>");
/// assert_eq!(into_keyword(token3), ElmToken::Operator(String::from("<*|>")));
/// ```
/// # Panics
/// When `text_token` is neither a keyword or name token (typically
/// empty strings or strings with whitespaces will raise a panic)
///
/// ```ignore
/// let bad_token1 = String::from("");
/// into_keyword(bad_token1);
///
/// let bad_token2 = String::from(" filter (+)");
/// into_keyword(bad_token2)
/// ```
fn into_keyword(text_token : String) -> ElmToken {
    match text_token.as_ref() {
        "module" => return ElmToken::Module,
        "exposing" => return ElmToken::Exposing,
        "import" => return ElmToken::Import,
        "as" => return ElmToken::As,
        ".." => return ElmToken::Ellision,
        "λ" => return ElmToken::Lambda,
        "\\" => return ElmToken::Lambda,
        "->" => return ElmToken::RArrow,
        "case" => return ElmToken::Case,
        "of" => return ElmToken::Of,
        "if" => return ElmToken::If,
        "then" => return ElmToken::Then,
        "else" => return ElmToken::Else,
        "|" => return ElmToken::Pipe,
        "=" => return ElmToken::Assign,
        ":" => return ElmToken::Colon,
        "type" => return ElmToken::Type,
        "infixr" => return ElmToken::Infixr,
        "infixl" => return ElmToken::Infixl,
        "port" => return ElmToken::Port,
        "where" => return ElmToken::Where,
        "let" => return ElmToken::Let,
        "in" => return ElmToken::In,
        _ => {},
    };
    if is_operator(first_char!(text_token)) {
        ElmToken::Operator(text_token)
    } else if first_char!(text_token).is_alphabetic() {
        ElmToken::Name(text_token)
    } else if first_char!(text_token).is_digit(10) {
        ElmToken::Number(text_token)
    } else {
        panic!(format!("#{}# was not a recognizable token!", text_token))
    }
}


/// Consumes the content of input that is necessary to build up the full
/// name. Builds up a string into the input "into", until "predicate"
/// solves.
///
/// Returns the number of consumed characters.
/// NOTE: on the *consume* functions: they work without the
/// "opening" character.
fn consume_into<I>(
    input: &mut Peekable<I>,
    into: &mut String,
    predicate: fn(char) -> bool
) -> u16
    where I: Iterator<Item=char>
{
    let mut consumed_chars = 0;
    while let Some(&c) = input.peek() {
        if predicate(c) {
            consumed_chars += 1;
            input.next();
            into.push(c);
        } else {
            return consumed_chars
        }
    }
}

/// Consumes the rest of a line until '\n' (consuming)
/// Also returns when reaching the end of the file
fn consume_line_comment(input: &mut Iterator<Item=char>) {
    loop {
        match input.next() {
            Some('\n') => return,
            None => return,
            _ => {},
        }
    }
}

/// Consumes a block comment. Handles properly nested
/// block comments. It returns a string if the comment
/// was a doc comment.
///
/// ## Returns
/// `(Some(docContent), #linesConsumed, columnEnd)`
///
/// where:
/// #linesConsumed is the number of line that has been consumed
/// columnEnd is special:
/// * if #linesConsumed is 0, then it is the number of characters consumed
/// * otherwise, it is the column at which the comment block finishes
///
/// say you have a block like this:
/// ```
/// {- Some
/// comment -}
/// ```
/// columnEnd is 10, the column at which the comment end
///
/// if you have:
/// ```
/// let {- Important variable! -} x = 10 in x * x
/// ```
/// columnEnd is 24: how many characters were consumed.
///
/// docContent is the string content of the comment, if it is
/// a doc comment, otherwise it is None.
///
/// If the end of file is hit, returns None
fn consume_block_comment<I>(input: &mut Peekable<I>)
    -> Option<(Option<String>, u16, u16)>
    where I: Iterator<Item=char>
{
    let mut last = '\0';
    let mut depth = 1i64;
    let mut nl_consumed = 0;
    let mut column = 1;
    let is_doc = {
        let second = input.next()?;
        let third = *input.peek()?;
        second == '-' && third == '|'
    };
    let mut accum = String::new();
    if is_doc {input.next().unwrap()} //We checked this is a "|"
    while c = input.next()? {
        nl_consumed += (c == '\n') as u16;
        column = (column + 1) * (c != '\n') as u16;
        if last == '{' && c == '-' {
            depth += 1
        } else if last == '-' && c == '}' {
            depth -= 1
        }
        if depth == 0 {
            accum.pop().unwrap(); // should be the last -
            return Some((
                if is_doc {Some(accum)} else {None},
                nl_consumed,
                column
            ))
        }
        accum.push(c);
        last = c;
    }
    None // Reached EOF before finishing the block comment
}

fn consume_operator<I>(input: &mut Peekable<I>, from: char) -> (String,u16)
    where I: Iterator<Item=char>
{
    let mut ret = String::new();
    ret.push(from);
    let consumed_chars = consume_into(input, into, is_operator);
    (ret,consumed_chars)
}

fn consume_name<I>(input: &mut Peekable<I>, from: char) -> (String,u16)
    where I: Iterator<Item=char>
{
    let mut ret = String::new();
    ret.push(from);
    let consumed_chars
        = consume_into(input, ret, |c| c.is_alphanumeric()||c=='.'||c=='_');
    (ret,consumed_chars)
}

/// Consumes a number literal. A number literal can be one of the
/// following:
/// ```
/// x[0-9a-fA-F]+
/// [0-9]*(.[0-9]+)?(e[-+]?[0-9]+)?
/// ```
///
/// ## Returns
/// Ok(NumberString, #consumedChar) if succesful,
/// Err(LexError) in case of error.
fn consume_number<I>(input: &mut Peekable<I>, from: char)
    -> Result<String,LexError>
    where I: Iterator<Item=char>
{
    let mut into = String::new();
    into.push(from);

    let is_hexa = match input.peek() { Some(&'x') => true, _ => false };
    if is_hexa {
        if from != '0' {
            return Err(LexError::Number(
                "an hexadecimal number starts with 0x\
                 and nothing else"
            ))
        }
        let next_char = match input.next() {
            Some(val) => val,
            None => return Err(LexError::Number(
                    "Expected an hexadecimal digit after 0x\
                     but got nothing."
            )),
        };
        into.push(next_char);
        consume_into(input, into, |c| c.is_digit(16));
        Ok(into)
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
                _ => return Ok(into),
            }
            input.next();
            into.push(c);
        }
        Ok(into)
    }
}

/// More or less makes sure that it is a valid character literal.
/// It won't check that \x... escapes are within reasonable bounds of
/// Unicode world.
///
/// ## Errors
/// - When EOF is reached before the end of the character
/// - When the literal is invalid
///
/// ## Notes
/// Doesn't include the closing ' in the `into` String and
/// assumes the opening ' has been consumed
fn consume_char(input: &mut Iterator<Item=char>)
    -> Result<String,LexError>
{
    macro_rules! finish_char {
        () => ({
            input.as_ref().take_while(|x| x != '\'').for_each(|_|{});
            Err(LexError::Char("Character token is too big"))
        });
        (EOF) => (Err(LexError::Char("Reached EOF while lexing a char")));
        ($msg:expr) => ({
            input.as_ref().take_while(|x| x != '\'').for_each(|_|{});
            Err(LexError::Char($msg))
        })
    }
    match input.next() {
        Some('\\') => {},
        Some('\n') => return
            finish_char!("Newlines are not allowed in character literals"),
        Some(c) => {
            return match input.next() {
                Some('\'') => Ok(format!("{}", c)),
                Some(_) => finish_char!(),
                None => finish_char!(EOF),
            }
        },
        None => return finish_char!(EOF),
    };
    match input.next() {
        Some(c) if c == '0' || c == 'x' => {
            let mut acc = format!("{}", c);
            loop { match input.next() {
                Some('\'') => return Ok(acc),
                Some(c) if c.is_digit(16) => acc.push(c) ,
                Some(_) => return finish_char!("Invalid character token"),
                None => return finish_char!(EOF),
            }}
        },
        Some(c) => {
            match input.next() {
                Some('\'') => Ok(format!("{}", c)),
                Some(_) => finish_char!(),
                None => finish_char!(EOF),
            }
        },
        None => finish_char!(EOF),
    }
}

/// Discriminates between triple-quoted string literals and
/// simply-quoted ones, returns the content of the literal.
///
/// ## Errors
/// - When EOF is reached before the end of the string.
/// - When there is a line return inside a simply-quoted string.
///
/// ## Notes
/// Doesn't include the closing " in the `into` String,
/// assumes the first " was consumed.
fn consume_string<I>(input: &mut Peekable<I>)
    -> Result<(String, u16, u16),LexError>
    where I:Iterator<Item=char>
{
    macro_rules! finish_string {
        (EOF)=>(Err(LexError::StringLit("Reached EOF while lexing a string")));
        ($msg:expr) => ( {
            input.as_ref().take_while(|x| x != '"').for_each(|_|{});
            Err(LexError::StringLit($msg))
        } )
    }
    let mut acc = String::new();
    let mut prev_is_escape = false;
    let mut nl_consumed = 0;
    let mut column = 0;
    let is_multiline = {
        let second = input.next()?;
        let third = *input.peek()?;
        second == '"' && third == '"'
    };
    loop {
        let next_char = input.next();
        match next_char {
            Some('\n') if !is_multiline => return finish_string!(
                "End of line in simply-quoted string literal"
            ),
            Some('"') if !is_multiline && !prev_is_escape => {
                return Ok(acc, nl_consumed, column)
            },
            Some('\\') => {
                prev_is_escape = !prev_is_escape;
            },
            None => finish_string!(EOF),
        }
        acc.push(next_char);
    }

    /*{{  i
        nl_consumed += (c == '\n') as u16;
        if c == '"' && !prev_is_escape {
            return nl_consumed;
        }
        prev_is_escape = c == '\\' && !prev_is_escape;
        acc.push(c);
    }}*/
}

/// Consumes up to the first non-whitespace. returns the proper
/// newline token If there is extra newlines before the next
/// token, the Token is created accordingly.
///
/// # Returns
/// `(Some(parsed_token), #lines_consumed, current_column)`
/// The token is a None if we reach the last line of the file.
fn consume_newline<I>(input: &mut Peekable<I>, line: u16)
    -> (Option<ElmToken>, u16, u16)
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
                     Some(ElmToken::Newline(
                         this_line as i16,
                         current_column as i16
                     )),
                     nl_consumed,
                     current_column
                )
            },
        };
        input.next();
    }
    (None, nl_consumed, 0)
}


/// An elm lexer.
///
/// The lexer is a statefull adaptator that transforms
/// a stream of characters into a stream of elm tokens.
/// A token is a data structures to ease parsing.
///
/// The lexer is an iterator, and does everything as
/// tokens are requested from it. This is to offer
/// the user most freedom about how they consume
/// streams.
///
/// You could collect into a vector all the tokens
/// before processing, or you could defer up to the
/// last moment the evaluation of tokens.
pub struct Lexer<I: Iterator<Item=char>> {
    input : Peekable<I>,
    line_loc : u16,
    column_loc : u16
}

/// Implements the ability to convert a character stream
/// into a token stream.
///
/// # Examples
///
/// ```rust
/// use elm_eureka::lexer::LexableIterator;
/// let elm_source = "f x = λy -> x ** y";
/// let lexer = elm_source.chars().lex();
/// ```
pub trait LexableIterator: Iterator<Item=char> {
    /// Creates a new lexer wrapping the
    /// character stream given as input.
    fn lex(self) -> Lexer<Self> where Self: Sized {
        Lexer {
            input: self.peekable(),
            line_loc: 1,
            column_loc: 0,
        }
    }
}
impl<T: Iterator<Item=char>> LexableIterator for T {}


impl <I: Iterator<Item=char>> Iterator for Lexer<I> {
    type Item = Lexed;

    /// Spews tokens while consuming the input stream.
    /// Behaves nicely on a file without syntax errors.
    ///
    /// # Panics
    /// The iterator may panic in two circumstances:
    ///
    /// * Somehow, the lexer tried to turn into a
    ///   token something that it shouldn't
    ///
    /// # Examples
    /// ```rust
    /// use elm_eureka::lexer::LexableIterator;
    /// use elm_eureka::ElmToken;
    /// use elm_eureka::ElmToken::{Name,Lambda,Assign,Operator,RArrow,StringLit,Number,Newline};
    /// let elm_source = "f x = λy -> x ** y
    /// g x = \"string\" ++ toString <| x * 9";
    /// let lexer = elm_source.chars().lex();
    /// let tokenized_source
    ///     = lexer
    ///         .map(Lexed::token)
    ///         .map(Option::unwrap)
    ///         .collect::<Vec<ElmToken>>();
    /// assert_eq!(tokenized_source, vec![
    ///     Name(String::from("f")),
    ///     Name(String::from("x")),
    ///     Assign,
    ///     Lambda,
    ///     Name(String::from("y")),
    ///     RArrow,
    ///     Name(String::from("x")),
    ///     Operator(String::from("**")),
    ///     Name(String::from("y")),
    ///     Newline(2,0),
    ///     Name(String::from("g")),
    ///     Name(String::from("x")),
    ///     Assign,
    ///     StringLit(String::from("string")),
    ///     Operator(String::from("++")),
    ///     Name(String::from("toString")), Operator(String::from("<|")),
    ///     Name(String::from("x")),
    ///     Operator(String::from("*")),
    ///     Number(String::from("9")),
    /// ]);
    /// ```
    ///
    /// # Errors
    /// The stream may terminate early for
    /// undeterminate reasons, mostly broken code
    fn next(&mut self) -> Option<Lexed> {
        macro_rules! make_lexed {
            ($token:expr; $beginline:expr; $begincol:expr) => (
                return Some(Lexed(Ok((
                        ($beginline, $begincol),
                        $token,
                        (self.line_loc, self.column_loc)))))
            )
        }
        macro_rules! make_char_lexed { ($token:ident) => (
            make_lexed!(ElmToken::$token; self.line_loc; self.column_loc - 1)
        ) }
        macro_rules! map_literal { ($consumer:expr, $token:ident) => (
            return Some($consumer.map(|consume_res| {
                self.column_loc += consume_res.len() - 1;
                ( (self.line_loc, self.column_loc - 1),
                  ElmToken::$token(consume_res),
                  (self.line_loc, self.column_loc + consume_res.len()-1))
            }))
        ) }

        self.column_loc += 1;
        { let input = &mut self.input; match input.next()? {
            '\r' => {},
            '\n' => {
                let (token, nl_consumed, column)
                    = consume_newline(input, self.line_loc + 1);
                self.line_loc += nl_consumed + 1;
                self.column_loc = column;
                return token.map(|t| Lexed(Ok((
                    (self.line_loc, self.column_loc),
                    t,
                    (self.line_loc, self.column_loc)
                ))))
            },
            c if c.is_whitespace() => {},
            '[' => make_char_lexed!(LBracket),
            ']' => make_char_lexed!(RBracket),
            '(' => make_char_lexed!(LParens),
            ')' => make_char_lexed!(RParens),
            ',' => make_char_lexed!(Comma),
            '{' => {
                if input.peek().map_or(false, |&c| c == '-') {
                    match consume_block_comment(input) {
                        (Some(doc), nl_consumed) => {
                            self.line_loc += nl_consumed;
                            make_lexed!(doc, )
                        },
                        (None, nl_consumed) =>
                            self.line_loc += nl_consumed,
                    }
                } else {
                    make_char_lexed!(LBrace)
                }
            },
            '}' => make_char_lexed!(RBrace),
            '.' => {
                if input.peek().map_or(false, |&c| is_operator(c)) {
                    let mut into_buffer = String::from(".");
                    consume_operator(input, &mut into_buffer);
                    ret!(into_keyword(into_buffer))
                } else if input.peek().map_or(false, |&c| c.is_alphabetic()) {
                    let mut into_buffer = String::from(".");
                    consume_name(input, &mut into_buffer);
                    ret!(ElmToken::Name(into_buffer))
                }
            },
            c if is_operator(c) => {
                if c == '-' && input.peek().map_or(false, |&c| c == '-') {
                    consume_line_comment(input);
                    self.line_loc += 1;
                    self.column_loc = 0;
                } else {
                    let mut into_buffer : String = to_str!(c);
                    consume_operator(input, &mut into_buffer);
                    ret!(into_keyword(into_buffer));
                }
            },
            c if c.is_alphabetic() => {
                let mut into_buffer : String = to_str!(c);
                consume_name(input, &mut into_buffer);
                ret!(into_keyword(into_buffer))
            },
            '_' => make_char_lexed!(Underscore),
            '"' => {
                let mut into_buffer = String::new();
                let nl_consumed = consume_string(input, &mut into_buffer);
                self.line_loc += nl_consumed;
                ret!(ElmToken::StringLit(into_buffer))
            },
            '\'' => map_literal!(consume_char(input), Char),
            c @ '0' ... '9' => map_literal!(consume_number(input,c), Number),
            c => {
                println!("######{}#####", c);
                return None
            },
        } }
        self.next()
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
import Html.Attributes exposing (class, Stuff(..))


{-| This {- Hello :)-}

Et maintenant le voyage au supermarché!
-}
main =
  ul [class "grocery-list"]
    [ li [] [.model >> text "Pamplemousse"]
    , li [] [Name.Wow << text "Ananas"]
    , li [] [text 103 "Jus d'orange"]
    , li [] [text "Bœuf"]
    ]


-- Thanks to "Flight of the Conchords" for creating "Foux Du Fafa"
"#;

    #[test] fn test_lexer() {
        use tokens::ElmToken::*;

        let str_input = String::from(UNORDERED_LIST_EXAMPLE);
        let lexer = str_input.chars().lex();
        let token_vec = lexer.collect::<Vec<_>>();
        assert_eq!(
            token_vec, vec![Import, s!(Name,"Html"), Exposing, LParens,
            s!(Name,"li"), Comma, s!(Name,"text"), Comma, s!(Name,"ul"),
            RParens, Newline(2, 0), Import, s!(Name,"Html.Attributes"),
            Exposing, LParens, s!(Name,"class"), Comma, s!(Name, "Stuff"),
            LParens, Ellision, RParens, RParens, Newline(5,0),
            DocComment(String::from(r#" This {- Hello :)-}

Et maintenant le voyage au supermarché!
"#)), Newline(9,0), s!(Name,"main"), Assign,
            Newline(10,2), s!(Name,"ul"), LBracket, s!(Name,"class"), s!(StringLit,"grocery-list"), RBracket,
            Newline(11,4),LBracket,s!(Name,"li"),LBracket, RBracket,
            LBracket, s!(Name, ".model"), s!(Operator, ">>"),
            s!(Name,"text"), s!(StringLit,"Pamplemousse"), RBracket,
            Newline(12,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name, "Name.Wow"), s!(Operator, "<<"),
            s!(Name,"text"), s!(StringLit,"Ananas"), RBracket,
            Newline(13,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(Number, "103"), s!(StringLit,"Jus d'orange"), RBracket,
            Newline(14,4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Bœuf"), RBracket,
            Newline(15,4), RBracket, Newline(18,0)]);
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
        consumer_test!( "Some.test-10", "Some.test", "-10", consume_name);
        consumer_test!( "Some.Test.hello", "Some.Test.hello","", consume_name);
        consumer_test!( ".hello", ".hello", "", consume_name);
        consumer_test!( "model.hello", "model.hello", "", consume_name);
    }

    macro_rules! block_test_boilerplate {
        ($input_value:expr, $input_remain:expr, $buffer_res:expr) => (
            let mut input = $input_value.chars().peekable();
            let (content, _) = consume_block_comment(&mut input);
            let expected_output = Some(ElmToken::DocComment(String::from(
                $buffer_res
            )));
            assert_eq!(content, expected_output);
            assert_eq!(input.collect::<String>(), $input_remain);
        );
        ($input_value:expr, $input_remain:expr) => (
            let mut input = $input_value.chars().peekable();
            let (content, _) = consume_block_comment(&mut input);
            let expected_output = None;
            assert_eq!(content, expected_output);
            assert_eq!(input.collect::<String>(), $input_remain);
        )
    }
    #[test] fn test_consume_block_comment1() {
        block_test_boilerplate!("--}float", "float");
    }
    #[test] fn test_consume_block_comment2() {
        block_test_boilerplate!("-hello world!!-}float", "float");
    }
    #[test] fn test_consume_block_comment3() {
        block_test_boilerplate!("-hello {-world!!-}float-}remains", "remains");
    }
    #[test] fn test_consume_block_comment4() {
        block_test_boilerplate!("-|-}float", "float", "");
    }
    #[test] fn test_consume_block_comment5() {
        block_test_boilerplate!(
            "-|hello {- hi-} world!!-}float",
            "float",
            "hello {- hi-} world!!"
        );
    }
    #[test] fn test_consume_block_comment6() {
        block_test_boilerplate!(
            "-|héllö {- hî-} wörld!ŵ\n-}ẑloat",
            "ẑloat",
            "héllö {- hî-} wörld!ŵ\n"
        );
    }

    #[test] fn test_into_keyword1() {
        let token = String::from("then");
        assert_eq!(into_keyword(token), ElmToken::Then);
    }
    #[test] fn test_into_keyword2() {
        let token = String::from("effect");
        assert_eq!(into_keyword(token), ElmToken::Name(String::from("effect")));
    }
    #[test] fn test_into_keyword3() {
        let token = String::from("<*|>");
        assert_eq!(into_keyword(token), ElmToken::Operator(String::from("<*|>")));
    }
    #[test] #[should_panic] fn test_into_keyword4() {
        let bad_token = String::from("");
        into_keyword(bad_token);
    }
    #[test] #[should_panic] fn test_into_keyword5() {
        let bad_token = String::from(" filter (+)");
        into_keyword(bad_token);
    }
}
