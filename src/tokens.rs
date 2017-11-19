//! A token is a convinient data structures for parsing.
//!
//! A source file can be represented as a sequence of
//! tokens. Tokens result from a simple processing of
//! source and do not give insight into the structure
//! of the code. It is mostly an intermediate representation
//! meant to be consumed by a parser.

use std::fmt;

/// Tokens that you can find in a source file.
///
/// This is not based on any elm specification, just my
/// guts.
///
/// One can convert back a list of tokens into source code
/// by using the `Display` trait. Everything up  to the
/// indentation should be left intact. Only loss would be
/// whitespaces and line/block comments.
#[derive(PartialEq)]
pub enum ElmToken {
    /// line, column coordinate of token starting a new line.
    Newline(i16,i16),
    /// An opening parenthesis `(`
    LParens,
    /// A closing parenthesis `)`
    RParens,
    /// A separating comma (`,`)
    Comma,
    /// An operator, follows the pattern:
    /// `[+-*/.!:=<>|&%^#$?@~\]+`
    Operator(String),
    /// Implicit exports (`exposing (..)`)
    Ellision,
    /// A variable or type name. `Can.Have.Prefix`
    Name(String),
    /// the `module` keyword.
    Module,
    /// the `exposing` keyword.
    Exposing,
    /// the `import` keyword.
    Import,
    /// the `as` keyword.
    As,
    /// An opening curly brace `{`
    LBrace,
    /// A closing curly brace `}`
    RBrace,
    /// A docstring. (starts by `{-|`)
    DocComment(String),
    /// An opening bracket `[` (for lists)
    LBracket,
    /// A closing bracket `]` (for lists)
    RBracket,
    /// the `::` operator, meaningfull in pattern matches
    Concat,
    /// the `\` in an anonymous function
    /// `(\{x, y} -> (x, y))`
    Lambda,
    /// the `->` in function types, lambdas and case
    /// branches.
    RArrow,
    /// the `case` keyword in `case .. of` expressions
    Case,
    /// the `of` keyword in  `case .. of` expressions
    Of,
    /// the `_` in pattern matches
    Underscore,
    /// the `if` keyword in `if .. then .. else ..`
    /// expressions
    If,
    /// the `then` keyword in `if .. then .. else ..`
    /// expressions
    Then,
    /// the `else` keyword in `if .. then .. else ..`
    /// expressions
    Else,
    /// not `|>` but, `|`, alternatives in union types
    Pipe,
    /// the equal sign :D
    Assign,
    /// the `:` colon
    TypeDeclr,
    /// the `type` keyword.
    Type,
    /// the `alias` keyword.
    Alias,
    /// the `infixr` keyword.
    Infixr,
    /// the `infixl` keyword.
    Infixl,
    /// module qualifier at the beginning of the file
    /// or function qualifier in port modules.
    Port,
    /// It is a valid name for literals, but also a keyword
    /// This offers a challenge for clean lexing. I might change
    /// how this works.
    Effect,
    /// comments and subscriptions type specification in
    /// `effect` modules.
    Where,
    /// The `let` keyword, for `let .. in` expressions
    Let,
    /// The `in` keyword, for `let .. in` expressions
    In,
    /// A string literal.
    StringLit(String),
    /// A number literal
    ///
    /// Note: some cases that shouldn't be accepted are currently
    /// accepted, such as `[0-9]x[0-9a-fA-F]` and `10.`
    Number(String),
    /// A character literal.
    ///
    /// Note: the lexer uses the same code for String literals
    /// and character literals, with ' and " swapped. I couldn't
    /// find a refer
    /// elm has `\0999999` (decimal escape) and `\xfffff` (hex escape)
    /// but also, one can escape some single-characters.
    Char(String),
}

impl fmt::Display for ElmToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ElmToken::Newline(_,column) =>
                write!(f, "\n{:width$}", "", width=(column as usize)),
            &ElmToken::LParens => write!(f, "("),
            &ElmToken::RParens => write!(f, ")"),
            &ElmToken::Comma => write!(f, ","),
            &ElmToken::Operator(ref op) => write!(f, "{}", op),
            &ElmToken::Ellision => write!(f, ".."),
            &ElmToken::Name(ref name) => write!(f, "{}", name),
            &ElmToken::Module => write!(f, "module"),
            &ElmToken::Exposing => write!(f, "exposing"),
            &ElmToken::Import => write!(f, "import"),
            &ElmToken::As => write!(f, "as"),
            &ElmToken::DocComment(ref content) => write!(f,"{{-|{}-}}",content),
            &ElmToken::LBrace => write!(f, "{{"),
            &ElmToken::RBrace => write!(f, "}}"),
            &ElmToken::LBracket => write!(f, "["),
            &ElmToken::RBracket => write!(f, "]"),
            &ElmToken::Concat => write!(f, "::"),
            &ElmToken::Lambda => write!(f, "\\"),
            &ElmToken::RArrow => write!(f, "->"),
            &ElmToken::Case => write!(f, "case"),
            &ElmToken::Of => write!(f, "of"),
            &ElmToken::Underscore => write!(f, "_"),
            &ElmToken::If => write!(f, "if"),
            &ElmToken::Then => write!(f, "then"),
            &ElmToken::Else => write!(f, "else"),
            &ElmToken::Pipe => write!(f, "|"),
            &ElmToken::Assign => write!(f, "="),
            &ElmToken::TypeDeclr => write!(f, ":"),
            &ElmToken::Type => write!(f, "type"),
            &ElmToken::Alias => write!(f, "alias"),
            &ElmToken::Infixr => write!(f, "infixr"),
            &ElmToken::Infixl => write!(f, "infixl"),
            &ElmToken::Port => write!(f, "port"),
            &ElmToken::Effect => write!(f, "effect"),
            &ElmToken::Where => write!(f, "where"),
            &ElmToken::Let => write!(f, "let"),
            &ElmToken::In => write!(f, "in"),
            &ElmToken::StringLit(ref content) => write!(f, "\"{}\"", content),
            &ElmToken::Number(ref content) => write!(f, "{}", content),
            &ElmToken::Char(ref content) => write!(f, "'{}'", content),
        }
    }
}
impl fmt::Debug for ElmToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ElmToken::Newline(line,column) => write!(f, "({},{})",line,column),
            &ElmToken::LParens => write!(f, "<(>"),
            &ElmToken::RParens => write!(f, "<)>"),
            &ElmToken::Comma => write!(f, "<,>"),
            &ElmToken::Operator(ref op) => write!(f, "${}$", op),
            &ElmToken::Ellision => write!(f, "<..>"),
            &ElmToken::Name(ref name) => write!(f, "_{}_", name),
            &ElmToken::Module => write!(f, "<module>"),
            &ElmToken::Exposing => write!(f, "<exposing>"),
            &ElmToken::Import => write!(f, "<import>"),
            &ElmToken::As => write!(f, "<as>"),
            &ElmToken::DocComment(_) => write!(f, "<{{-| ... -}}>"),
            &ElmToken::LBrace => write!(f, "<{{>"),
            &ElmToken::RBrace => write!(f, "<}}>"),
            &ElmToken::LBracket => write!(f, "<[>"),
            &ElmToken::RBracket => write!(f, "<]>"),
            &ElmToken::Concat => write!(f, "<::>"),
            &ElmToken::Lambda => write!(f, "<λ>"),
            &ElmToken::RArrow => write!(f, "<->>"),
            &ElmToken::Case => write!(f, "<case>"),
            &ElmToken::Of => write!(f, "<of>"),
            &ElmToken::Underscore => write!(f, "<_>"),
            &ElmToken::If => write!(f, "<if>"),
            &ElmToken::Then => write!(f, "<then>"),
            &ElmToken::Else => write!(f, "<else>"),
            &ElmToken::Pipe => write!(f, "<|>"),
            &ElmToken::Assign => write!(f, "<=>"),
            &ElmToken::TypeDeclr => write!(f, "<:>"),
            &ElmToken::Type => write!(f, "<type>"),
            &ElmToken::Alias => write!(f, "<alias>"),
            &ElmToken::Infixr => write!(f, "<infixr>"),
            &ElmToken::Infixl => write!(f, "<infixl>"),
            &ElmToken::Port => write!(f, "<port>"),
            &ElmToken::Effect => write!(f, "<effect>"),
            &ElmToken::Where => write!(f, "<where>"),
            &ElmToken::Let => write!(f, "<let>"),
            &ElmToken::In => write!(f, "<in>"),
            &ElmToken::StringLit(_) => write!(f, "\"..\""),
            &ElmToken::Number(ref content) => write!(f, "#{}#", content),
            &ElmToken::Char(ref content) => write!(f, "'{}'", content),
        }
    }
}

