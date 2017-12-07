//! A lexer to tokenize elm source code
//!
//! Currently, tokenizes most valid elm code, but also
//! a lot of invalid elm code. One soar point is the
//! handling of literals.
//!
//! Currently multiline string literals are not
//! lexed correctly.
//!
//! Indentation at newline is kept track of with the
//! Newline token.

use std::iter::Peekable;
use tokens::ElmToken;

fn is_operator(symbol : char) -> bool {
    match symbol {
        '+' | '-' | '*' | '/' | '.' | 'λ' |
        '!' | ':' | '=' | '<' | '>' |
        '|' | '&' | '%' | '^' | '#' |
        '$' | '?' | '@' | '~' | '\\' => true,
        _ => false
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
    loop {
        match input.peek() {
            Some(&c) if predicate(c) => into.push(input.next().unwrap()),
            _ => break,
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
        let _ = input.next().unwrap();
    }
}

/// Consumes a block comment. Handles properly nested
/// block comments. This function doesn't accumulate
/// the input into a buffer. Instead, it returns a
/// string if the comment was a doc comment.
///
/// returns`(Some(DocCommentToken), #linesConsumed)`
fn consume_block_comment<I>(input: &mut Peekable<I>) -> (Option<ElmToken>, i16)
    where I: Iterator<Item=char>
{
    let mut last = '\0';
    let mut depth = 1i64;
    let mut nl_consumed = 0;
    let is_doc = {
        let second = match input.next() {
            Some(character) => character,
            None => return (None, 0),
        };
        let third = match input.peek() {
            Some(&character) => character,
            None => return (None, 0),
        };
        second == '-' && third == '|'
    };
    if is_doc {
        let mut accum = String::new();
        input.next().unwrap(); //We checked this is a "|"
        while let Some(c) = input.next() {
            nl_consumed += (c == '\n') as i16;
            if last == '{' && c == '-' {
                depth += 1
            } else if last == '-' && c == '}' {
                depth -= 1
            };
            if depth == 0 {
                accum.pop().unwrap(); // should be the last -
                return (Some(ElmToken::DocComment(accum)), nl_consumed)
            }
            accum.push(c);
            last = c;
        }
        (None, nl_consumed)
    } else {
        while let Some(c) = input.next() {
            nl_consumed += (c == '\n') as i16;
            if last == '{' && c == '-' {
                depth += 1
            } else if last == '-' && c == '}' {
                depth -= 1
            };
            last = c;
            if depth == 0 { return (None, nl_consumed) }
        }
        (None, nl_consumed)
    }
}

fn consume_operator<I>(input: &mut Peekable<I>, from: char) -> ElmToken
    where I: Iterator<Item=char>
{
    let mut into = String::new();
    into.push(from);
    consume_into(input, &mut into, is_operator);
    match into.as_ref() {
        ".." => return ElmToken::Ellision,
        "λ" => return ElmToken::Lambda,
        "\\" => return ElmToken::Lambda,
        "->" => return ElmToken::RArrow,
        "|" => return ElmToken::Pipe,
        "=" => return ElmToken::Assign,
        ":" => return ElmToken::Colon,
        _ => {},
    }
    ElmToken::Operator(into)
}

fn consume_name<I>(input: &mut Peekable<I>, from: char) -> ElmToken
    where I: Iterator<Item=char>
{
    let mut into = String::new();
    into.push(from);
    consume_into(input, &mut into, |c| c.is_alphanumeric()||c=='.'||c=='_');
    match into.as_ref() {
        "module" => return ElmToken::Module,
        "exposing" => return ElmToken::Exposing,
        "import" => return ElmToken::Import,
        "as" => return ElmToken::As,
        "case" => return ElmToken::Case,
        "of" => return ElmToken::Of,
        "if" => return ElmToken::If,
        "then" => return ElmToken::Then,
        "else" => return ElmToken::Else,
        "type" => return ElmToken::Type,
        "infixr" => return ElmToken::Infixr,
        "infixl" => return ElmToken::Infixl,
        "infix" => return ElmToken::Infixl, //infix implies infixl
        "port" => return ElmToken::Port,
        "where" => return ElmToken::Where,
        "let" => return ElmToken::Let,
        "in" => return ElmToken::In,
        _ => {},
    }
    ElmToken::Name(into)
}

/// Consumes a number literal. A number literal can be one of the
/// following:
/// ```
/// x[0-9a-fA-F]+
/// [0-9]*(.[0-9]+)?(e[-+]?[0-9]+)?
/// ```
/// Currently, also accepts [0-9]x[0-9a-fA-F]+
fn consume_number<I>(input: &mut Peekable<I>, from: char) -> ElmToken
    where I: Iterator<Item=char>
{
    let mut into = String::new();
    into.push(from);
    let is_hexa = match input.peek() { Some(&'x') => true, _ => false };
    if is_hexa {
        into.push(input.next().unwrap());
        consume_into(input, &mut into, |c| c.is_digit(16));
        ElmToken::Number(into)
    } else {
        let mut radix = false;
        let mut has_digit = false;
        let mut just_radix = false;
        while let Some(&c) = input.peek() {
            match c {
                val if val.is_digit(10)           => radix = just_radix,
                '.' if !has_digit                 => has_digit = true,
                '+' | '-' if just_radix && !radix => radix = just_radix,
                'e' | 'E' if !just_radix => {
                    just_radix = true; has_digit = true
                },
                _ => return ElmToken::Number(into),
            }
            // If it was none, we would have broken out of the loop just before
            into.push(input.next().unwrap());
        }
        ElmToken::Number(into)
    }
}

/// Why use a string to represent the content of a char? well, escape
/// sequences, my dear!
/// This just makes sure that '\'' doesn't trip up the lexer
/// We intend to parse valid code anyway, we can accept garbage to an
/// extent.
/// NOTE: doesn't include the closing ' in the `into` String.
fn consume_char(input: &mut Iterator<Item=char>) -> ElmToken {
    let mut into = String::new();
    let mut prev_is_escape = false;
    loop {
        match input.next() {
            None => break ElmToken::Char(into),
            Some('\'') if !prev_is_escape => break ElmToken::Char(into),
            Some(c) => {
                prev_is_escape = c == '\\' && !prev_is_escape;
                into.push(c);
            },
        }
    }
}

/// This is the same parser as char, just ignores \"
/// NOTE: doesn't include the closing " in the `into` String.
fn consume_string(input: &mut Iterator<Item=char>)
    -> (ElmToken, i16)
{
    let mut into = String::new();
    let mut prev_is_escape = false;
    let mut nl_consumed = 0;
    while let Some(c) = input.next() {
        nl_consumed += (c == '\n') as i16;
        if c == '"' && !prev_is_escape {
            return (ElmToken::StringLit(into), nl_consumed)
        }
        prev_is_escape = c == '\\' && !prev_is_escape;
        into.push(c);
    }
    (ElmToken::StringLit(into), nl_consumed)
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
    input: Peekable<I>,
    line_loc: i16,
    last_white: bool,
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
            last_white: false,
        }
    }
}
impl<T: Iterator<Item=char>> LexableIterator for T {}

impl <I: Iterator<Item=char>> Iterator for Lexer<I> {
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
    /// # Examples
    /// ```rust
    /// use elm_eureka::lexer::LexableIterator;
    /// use elm_eureka::ElmToken;
    /// use elm_eureka::ElmToken::{Name,Lambda,Assign,Operator,RArrow,StringLit,Number,Newline};
    /// let elm_source = "f x = λy -> x ** y
    /// g x = \"string\" ++ toString <| x * 9";
    /// let lexer = elm_source.chars().lex();
    /// let tokenized_source = lexer.collect::<Vec<ElmToken>>();
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
    ///     Name(String::from("toString")),
    ///     Operator(String::from("<|")),
    ///     Name(String::from("x")),
    ///     Operator(String::from("*")),
    ///     Number(String::from("9")),
    /// ]);
    /// ```
    ///
    /// # Errors
    /// The stream may terminate early for
    /// undeterminate reasons, mostly broken code
    fn next(&mut self) -> Option<ElmToken> {
        let old_white = self.last_white;
        self.last_white = false;

        { let input = &mut self.input; match input.next()? {
            '\r' => {},
            '\n' => {
                let (token, nl_consumed)
                    = consume_newline(input, self.line_loc + 1);
                self.line_loc += nl_consumed + 1;
                self.last_white = true;
                return token
            },
            c if c.is_whitespace() => self.last_white = true,
            '[' => return Some(ElmToken::LBracket),
            ']' => return Some(ElmToken::RBracket),
            '(' => return Some(ElmToken::LParens),
            ')' => return Some(ElmToken::RParens),
            ',' => return Some(ElmToken::Comma),
            '{' => {
                if input.peek().map_or(false, |&c| c == '-') {
                    match consume_block_comment(input) {
                        (Some(doc), nl_consumed) => {
                            self.line_loc += nl_consumed;
                            return Some(doc)
                        },
                        (None, nl_consumed) => self.line_loc += nl_consumed,
                    }
                } else { return Some(ElmToken::LBrace) }
            },
            '}' =>
                return Some(ElmToken::RBrace),

            '.' => match input.peek() {
                Some(&c) if is_operator(c) =>
                    return Some(consume_operator(input, '.')),

                Some(&c) if c.is_alphabetic() =>
                    return Some(consume_name(input, '.')),

                _ => unreachable!(),
            },
            '-' => match input.peek() {
                    Some(&'-') =>
                        consume_line_comment(input),

                    Some(&c) if is_operator(c) =>
                        return Some(consume_operator(input, '-')),

                    Some(_) if !old_white =>
                        return Some(ElmToken::Operator(String::from("-"))),

                    Some(&c) if c.is_digit(10) =>
                        //FIXME: add proper handling for prefix '-' (requires
                        //patching the expression grammar). Currently,
                        //valid constructs such as 10 -(3 * 3) will blow up
                        return Some(consume_number(input, '-')),

                    Some(_) =>
                        return Some(ElmToken::Operator(String::from("-"))),

                    None => unreachable!(),
            },
            c if is_operator(c) =>
                return Some(consume_operator(input, c)),

            c if c.is_alphabetic() =>
                return Some(consume_name(input, c)),

            c @ '0' ... '9' =>
                return Some(consume_number(input, c)),

            '_' =>
                return Some(ElmToken::Underscore),

            '"' => {
                let (token, nl_consumed) = consume_string(input);
                self.line_loc += nl_consumed;
                return Some(token)
            },
            '\'' =>
                return Some(consume_char(input)),

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
        ($fn_to_call:expr, $str_to_conv:expr) => (
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

    // IMPORTANT NOTE: This test in very limited, there is a lot of ways to
    // invalidate the number lexer (notably `-0x499`), but this is not
    // an issue until I decide it is.
    #[test] fn number_lexing() {
        use tokens::ElmToken::*;
        let src = String::from("name-10e-10 10-10 10e+99 0x1e+99 0xabcd --0x449");
        let tknzd = src.chars().lex().collect::<Vec<_>>();
        assert_eq!(tknzd, vec![
            s!(Name,"name"), s!(Operator,"-"), s!(Number,"10e-10"),
            s!(Number,"10"), s!(Operator,"-"), s!(Number,"10"),
            s!(Number,"10e+99"), s!(Number,"0x1e"), s!(Operator,"+"),
            s!(Number,"99"), s!(Number,"0xabcd") ]);
    }

    macro_rules! consumer_test {
        ($input_value:expr, $expected_result:expr, $input_remain:expr, $to_test:ident) => (
            let mut input = $input_value.chars().peekable();
            let head =input.next().unwrap();
            let token = $to_test(&mut input, head);
            assert_eq!(token, $expected_result);
            assert_eq!(input.collect::<String>(), $input_remain);
        );
        (S, $input_value:expr, $expected_result:expr, $input_remain:expr, $to_test:ident) => (
            let mut input = $input_value.chars().peekable();
            input.next().unwrap();
            let token = $to_test(&mut input);
            assert_eq!(token, $expected_result);
            assert_eq!(input.collect::<String>(), $input_remain);
        )
    }

    #[test] fn test_consume_operator() {
        use tokens::ElmToken::Operator;
        consumer_test!( "+*--12", s!(Operator,"+*--"), "12", consume_operator);
        consumer_test!( "<*| wow |*>", s!(Operator,"<*|"), " wow |*>", consume_operator);
    }
    #[test] fn test_consume_char() {
        use tokens::ElmToken::Char;
        consumer_test!(S, r#"'\xfff' 10"#, s!(Char,r#"\xfff"#), " 10", consume_char);
        consumer_test!(S, r#"'\\' ten"#, s!(Char,r#"\\"#), " ten", consume_char);
        consumer_test!(S, r#"'\''''"#, s!(Char,r#"\'"#), "''", consume_char);
    }
    #[test] fn test_consume_number() {
        use tokens::ElmToken::Number;
        consumer_test!( "0xabcd-10", s!(Number,"0xabcd"), "-10", consume_number);
        consumer_test!( "10e+34-10", s!(Number,"10e+34"), "-10", consume_number);
        consumer_test!( "3.1415-10", s!(Number,"3.1415"), "-10", consume_number);
    }
    #[test] fn test_consume_name() {
        use tokens::ElmToken::{Name,Type};
        consumer_test!( "Some.test-10", s!(Name,"Some.test"), "-10", consume_name);
        consumer_test!( "type -10", Type, " -10", consume_name);
        consumer_test!( "Some.test-10", s!(Name,"Some.test"), "-10", consume_name);
        consumer_test!( "Some.Test.hello", s!(Name,"Some.Test.hello"),"", consume_name);
        consumer_test!( ".hello", s!(Name,".hello"), "", consume_name);
        consumer_test!( "model.hello", s!(Name,"model.hello"), "", consume_name);
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
}
