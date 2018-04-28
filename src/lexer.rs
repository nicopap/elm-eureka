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
use tokens::{ElmToken,Location};

type Consumed = (u32,u16);

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
fn consume_block_comment<I>(input: &mut Peekable<I>)
    -> (Consumed, Option<String>)
    where I:Iterator<Item=char>,
{
    let mut last = '\0';
    let mut depth = 1i64;
    let mut nl_consumed = 0;
    let mut column_shift = 1;
    let is_doc = {
        let second = match input.next() {
            Some('-') => '-',
            None => return ((0,1), None),
            _ => panic!("Called consume_block_comment on not block comment"),
        };
        let third = match input.peek() {
            Some(&character) => character,
            None => return ((0,1), None),
        };
        second == '-' && third == '|'
    };
    if is_doc {
        let mut accum = String::new();
        input.next().unwrap(); //We checked this is a "|"
        column_shift += 1;
        while let Some(c) = input.next() {
            column_shift += 1;
            if c == '\n' {
                nl_consumed += 1;
                column_shift = 1;
            }
            if last == '{' && c == '-' {
                depth += 1
            } else if last == '-' && c == '}' {
                depth -= 1
            };
            if depth == 0 {
                accum.pop().unwrap(); // should be the last -
                return ((nl_consumed, column_shift), Some(accum))
            }
            accum.push(c);
            last = c;
        }
        ((nl_consumed,column_shift), None)
    } else {
        while let Some(c) = input.next() {
            column_shift += 1;
            if c == '\n' {
                nl_consumed += 1;
                column_shift = 1;
            }
            if last == '{' && c == '-' {
                depth += 1
            } else if last == '-' && c == '}' {
                depth -= 1
            };
            last = c;
            if depth == 0 { break }
        }
        ((nl_consumed, column_shift), None)
    }
}

fn consume_operator<I>(input: &mut Peekable<I>, from: char) -> (u16, ElmToken)
    where I: Iterator<Item=char>
{
    use self::ElmToken::*;

    let mut into = String::new();
    into.push(from);
    consume_into(input, &mut into, is_operator);
    (into.len() as u16 - 1, match into.as_ref() {
        ".." => Ellision,
        "λ" | "\\" => Lambda,
        "->" => RArrow,
        "|" => Pipe,
        "=" => Assign,
        ":" => Colon,
        _ => Operator(into),
    })
}

fn consume_name<I>(input: &mut Peekable<I>, from: char) -> (u16, ElmToken)
    where I: Iterator<Item=char>
{
    use self::ElmToken::*;

    let mut into = String::new();
    into.push(from);
    consume_into(input, &mut into, |c| c.is_alphanumeric()||c=='.'||c=='_');
    (into.len() as u16 - 1, match into.as_ref() {
        "module" => Module,
        "exposing" => Exposing,
        "import" => Import,
        "as" => As,
        "case" => Case,
        "of" => Of,
        "if" => If,
        "then" => Then,
        "else" => Else,
        "type" => Type,
        "infixr" => Infixr,
        "infixl" => Infixl,
        "infix" => Infix,
        "port" => Port,
        "where" => Where,
        "let" => Let,
        "in" => In,
        _ => Name(into),
    })

}

/// Consumes a number literal. A number literal can be one of the
/// following:
/// `x[0-9a-fA-F]+` and `[0-9]*(.[0-9]+)?(e[-+]?[0-9]+)?`
/// Currently, also accepts `[0-9]x[0-9a-fA-F]+`
fn consume_number<I>(input: &mut Peekable<I>, from: char) -> (u16, ElmToken)
    where I: Iterator<Item=char>
{
    use self::ElmToken::Number;

    let mut into = String::new();
    into.push(from);
    let is_hexa = match input.peek() { Some(&'x') => true, _ => false };
    if is_hexa {
        into.push(input.next().unwrap());
        consume_into(input, &mut into, |c| c.is_digit(16));
        (into.len() as u16 - 1, Number(into))
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
                _ => return (into.len() as u16 - 1, Number(into)),
            }
            // If it was none, we would have broken out of the loop just before
            into.push(input.next().unwrap());
        }
        (into.len() as u16 - 1, Number(into))
    }
}

/// Why use a string to represent the content of a char? well, escape
/// sequences, my dear!
/// This just makes sure that '\'' doesn't trip up the lexer
/// We intend to parse valid code anyway, we can accept garbage to an
/// extent.
/// NOTE: doesn't include the closing ' in the `into` String.
fn consume_char(input: &mut Iterator<Item=char>) -> (u16, ElmToken) {
    use self::ElmToken::Char;

    let mut into = String::new();
    let mut prev_is_escape = false;
    loop {
        match input.next() {
            None => break (into.len() as u16 + 1, Char(into)),
            Some('\'') if !prev_is_escape => break (into.len() as u16 + 1, Char(into)),
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
    -> (Consumed, ElmToken)
{
    use self::ElmToken::StringLit;

    let mut into = String::new();
    let mut prev_is_escape = false;
    let mut nl_consumed = 0;
    let mut column_shift = 0;
    for c in input {
        column_shift += 1;
        if c == '\n' {
            nl_consumed += 1;
            column_shift = 1;
        }
        if c == '"' && !prev_is_escape {
            return ((nl_consumed, column_shift), StringLit(into))
        }
        prev_is_escape = c == '\\' && !prev_is_escape;
        into.push(c);
    }
    ((nl_consumed, column_shift), StringLit(into))
}

/// Consumes up to the first non-whitespace. returns the proper
/// newline token If there is extra newlines before the next
/// token, the Token is created accordingly.
fn consume_newline<I>(input: &mut Peekable<I>) -> (Consumed, bool)
    where I:Iterator<Item=char>,
{
    let mut nl_consumed = 0;
    let mut current_column = 0;
    while let Some(&c) = input.peek() {
        match c {
            ' ' => {current_column += 1},
            '\n' => {nl_consumed += 1; current_column = 0},
            _ => {
                return ((nl_consumed, current_column), true);
            },
        };
        input.next();
    }
    ((nl_consumed, current_column), false)
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
    line_no: u32,
    column_no: u16,
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
            line_no: 1,
            column_no: 1,
            last_white: false,
        }
    }
}
impl<T: Iterator<Item=char>> LexableIterator for T {}

impl <I: Iterator<Item=char>> Iterator for Lexer<I> {
    type Item = (Location,ElmToken);

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
    /// let lexer = elm_source.chars().lex().map(|(_p,t)| t);
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
    ///     Newline(0),
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
    fn next(&mut self) -> Option<(Location,ElmToken)> {
        use self::ElmToken::*;

        let old_white = self.last_white;
        let old_location = (self.line_no, self.column_no);
        self.last_white = false;

        macro_rules! ret {
            (single $ident:expr) => ({
                return Some((
                    (old_location, (self.line_no, self.column_no)),
                    $ident
                ))
            });
            ($charcons_token:expr) => ({
                let (consumed, token) = $charcons_token;
                self.column_no += consumed;
                return Some((
                    (old_location, (self.line_no, self.column_no)),
                    token
                ));
            });
        }

        self.column_no += 1;
        match self.input.next()? {
            '\r' => {},
            '\n' => {
                let (consumed, eof_not_reached) = consume_newline(&mut self.input);
                if !eof_not_reached {
                    return None
                } else {
                    self.last_white = true;
                    self.line_no += consumed.0 + 1;
                    self.column_no = consumed.1 + 1;
                    let new_location = (self.line_no, self.column_no);
                    return Some((
                        (new_location, new_location),
                        Newline(consumed.1 as u32)
                    ));
                }
            },
            c if c.is_whitespace() => {
                self.last_white = true;
            },
            '[' => ret!(single LBracket),
            ']' => ret!(single RBracket),
            '(' => ret!(single LParens),
            ')' => ret!(single RParens),
            ',' => ret!(single Comma),
            '{' => {
                if self.input.peek().map_or(false, |&c| c == '-') {
                    let (consumed, maybedoc) = consume_block_comment(&mut self.input);
                    self.line_no += consumed.0;
                    self.column_no =
                        (self.column_no * ((consumed.0 == 0) as u16))
                        + consumed.1;
                    if let Some(doc) = maybedoc {
                        return Some((
                            (old_location, (self.line_no,self.column_no)),
                            DocComment(doc)
                        ));
                    }
                } else {
                    ret!(single LBrace)
                }
            },
            '}' => ret!(single RBrace),

            '.' => match self.input.peek() {
                Some(&c) if is_operator(c) =>
                    ret!(consume_operator(&mut self.input, '.')),

                Some(&c) if c.is_alphabetic() =>
                    ret!(consume_name(&mut self.input, '.')),

                _ => unreachable!(),
            },
            '-' => match self.input.peek() {
                    Some(&'-') =>
                        consume_line_comment(&mut self.input),
                    Some(&c) if is_operator(c) =>
                        ret!(consume_operator(&mut self.input, '-')),
                    Some(_)  if !old_white =>
                        ret!((0, Operator("-".to_owned()))),
                    //FIXME: add proper handling for prefix '-' (requires
                    //patching the expression grammar). Currently,
                    //valid constructs such as 10 -(3 * 3) will blow up
                    Some(&c) if c.is_digit(10) =>
                        ret!(consume_number(&mut self.input, '-')),
                    Some(_) =>
                        ret!((0, Operator("-".to_owned()))),
                    None => unreachable!(),
            },
            c if is_operator(c) =>
                ret!(consume_operator(&mut self.input, c)),

            c if c.is_alphabetic() =>
                ret!(consume_name(&mut self.input, c)),

            c @ '0' ... '9' =>
                ret!(consume_number(&mut self.input, c)),

            '_' =>
                ret!(single Underscore),

            '"' => {
                let (consumed, token) = consume_string(&mut self.input);
                self.line_no += consumed.0;
                self.column_no =
                    (self.column_no * ((consumed.0 == 0) as u16)) + consumed.1;
                return Some((
                    (old_location, (self.line_no,self.column_no)),
                    token
                ));
            },
            '\'' =>
                ret!(consume_char(&mut self.input)),

            c => {
                println!("######{}#####", c);
                return None
            },
        }
        self.next()
    }
}

#[cfg(test)] mod tests {
    use super::*;
    use test::{Bencher,black_box};

    macro_rules! s {
        ($fn_to_call:ident, $str_to_conv:expr) => (
            $fn_to_call(String::from($str_to_conv))
        )
    }
    const UNORDERED_LIST_EXAMPLE : &str = include_str!("parser/elm_samples/unordered_list.elm");
    const HELLO_WORLD : &str = include_str!("parser/elm_samples/no_head.elm");

    #[test] fn test_lexer() {
        use tokens::ElmToken::*;
        let str_input = String::from(UNORDERED_LIST_EXAMPLE);
        let lexer = str_input.chars().lex().map(|(_p,t)| t);
        let token_vec = lexer.collect::<Vec<_>>();
        assert_eq!(
            token_vec, vec![Import, s!(Name,"Html"), Exposing, LParens,
            s!(Name,"li"), Comma, s!(Name,"text"), Comma, s!(Name,"ul"),
            RParens, Newline(0), Import, s!(Name,"Html.Attributes"),
            Exposing, LParens, s!(Name,"class"), Comma, s!(Name, "Stuff"),
            LParens, Ellision, RParens, RParens, Newline(0),
            DocComment(r#" This {- Hello :)-}

Et maintenant le voyage au supermarché!
"#.to_owned()), Newline(0), s!(Name,"main"), Assign,
            Newline(2), s!(Name,"ul"), LBracket, s!(Name,"class"), s!(StringLit,"grocery-list"), RBracket,
            Newline(4),LBracket,s!(Name,"li"),LBracket, RBracket,
            LBracket, s!(Name, ".model"), s!(Operator, ">>"),
            s!(Name,"text"), s!(StringLit,"Pamplemousse"), RBracket,
            Newline(4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name, "Name.Wow"), s!(Operator, "<<"),
            s!(Name,"text"), s!(StringLit,"Ananas"), RBracket,
            Newline(4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(Number, "103"), s!(StringLit,"Jus d'orange"), RBracket,
            Newline(4), Comma, s!(Name,"li"), LBracket, RBracket,
            LBracket, s!(Name,"text"), s!(StringLit,"Bœuf"), RBracket,
            Newline(4), RBracket, Newline(0)]);
    }

    #[test] fn test_hello_lex_location_tracking() {
        use tokens::ElmToken::*;
        let str_input = String::from(HELLO_WORLD);
        let lexer = str_input.chars().lex().map(|((p,q),t)|
            (p.0, p.1, q.0, q.1, t)
        );
        let token_vec = lexer.collect::<Vec<_>>();
        assert_eq!(token_vec, vec![
            (1 ,1 ,1 ,7 ,Import),           (1,8,1,12,s!(Name,"Html")),
            (1 ,13,1 ,21,Exposing),         (1 ,22,1 ,23,LParens),
            (1 ,23,1 ,27,s!(Name,"text")),  (1 ,27,1 ,28,RParens),
            (3 ,1 ,3 ,1 ,Newline(0)),
            (3 ,1 ,6 ,3 ,s!(DocComment,"Moduledoc\n\n\n")),
            (8 ,1 ,8 ,1 ,Newline(0)),
            (8 ,1 ,8 ,13,s!(DocComment,"Fundoc ")),
            (9 ,1 ,9 ,1 , Newline(0)), (9 ,1 ,9 ,5 ,s!(Name,"main")),
            (9 ,15,9 ,16,Assign),      (10,3 ,10,3 , Newline(2)),
            (10,3 ,10,7 ,s!(Name,"text")),
            (10,8 ,10,23,s!(StringLit,"Hello, World!"))
        ]);
    }

    // IMPORTANT NOTE: This test in very limited, there is a lot of ways to
    // invalidate the number lexer (notably `-0x499`), but this is not
    // an issue until I decide it is.
    #[test] fn number_lexing() {
        use tokens::ElmToken::*;
        let src = "name-10e-10 10-10 10e+99 0x1e+99 0xabcd --0x449".to_owned();
        let tknzd = src.chars().lex().map(|(_p,t)| t).collect::<Vec<_>>();
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
            let (_, token) = $to_test(&mut input, head);
            assert_eq!(token, $expected_result);
            assert_eq!(input.collect::<String>(), $input_remain);
        );
        (S, $input_value:expr, $expected_result:expr, $input_remain:expr, $to_test:ident) => (
            let mut input = $input_value.chars().peekable();
            input.next().unwrap();
            let (_, token) = $to_test(&mut input);
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
            let (_, content) = consume_block_comment(&mut input);
            let expected_output = Some(String::from($buffer_res));
            assert_eq!(content, expected_output);
            assert_eq!(input.collect::<String>(), $input_remain);
        );
        ($input_value:expr, $input_remain:expr) => (
            let mut input = $input_value.chars().peekable();
            let (_, content) = consume_block_comment(&mut input);
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

    #[bench] fn bench_lexer(bench: &mut Bencher) {
        let source_txt = include_str!("../examples/elmjutsu-5k.elm");
        bench.iter(|| {
            let owned_txt = source_txt.to_owned();
            let token_stream = owned_txt.chars().lex();
            black_box({token_stream.collect::<Vec<_>>();()})
        })
    }
}
