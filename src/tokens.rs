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
#[derive(PartialEq,Clone)]
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
    /// and character literals, with ' and " swapped.
    /// elm has `\0999999` (decimal escape) and `\xfffff` (hex escape)
    /// but also, one can escape some single-characters.
    Char(String),
    /// A significant indent in a let expression
    LetIndent,
    /// A significant indent in a case expression
    CaseIndent,
    /// Terminates a "case" expression when they are directly nested
    Endcase,
}

impl fmt::Display for ElmToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ElmToken::*;
        match *self {
            Newline(_,column) => write!(f,"\n{:w$}","", w=(column as usize)),
            LParens => write!(f, "("),
            RParens => write!(f, ")"),
            Comma => write!(f, ","),
            Operator(ref op) => write!(f, "{}", op),
            Ellision => write!(f, ".."),
            Name(ref name) => write!(f, "{}", name),
            Module => write!(f, "module"),
            Exposing => write!(f, "exposing"),
            Import => write!(f, "import"),
            As => write!(f, "as"),
            DocComment(ref content) => write!(f,"{{-|{}-}}",content),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            Lambda => write!(f, "\\"),
            RArrow => write!(f, "->"),
            Case => write!(f, "case"),
            Of => write!(f, "of"),
            Underscore => write!(f, "_"),
            If => write!(f, "if"),
            Then => write!(f, "then"),
            Else => write!(f, "else"),
            Pipe => write!(f, "|"),
            Assign => write!(f, "="),
            TypeDeclr => write!(f, ":"),
            Type => write!(f, "type"),
            Alias => write!(f, "alias"),
            Infixr => write!(f, "infixr"),
            Infixl => write!(f, "infixl"),
            Port => write!(f, "port"),
            Where => write!(f, "where"),
            Let => write!(f, "let"),
            In => write!(f, "in"),
            StringLit(ref content) => write!(f, "\"{}\"", content),
            Number(ref content) => write!(f, "{}", content),
            Char(ref content) => write!(f, "'{}'", content),
            LetIndent => write!(f,""),
            CaseIndent => write!(f,""),
            Endcase => write!(f,""),
        }
    }
}

impl fmt::Debug for ElmToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ElmToken::*;
        match *self {
            Newline(line,column) => write!(f, "({},{})",line,column),
            LParens => write!(f, "<(>"),
            RParens => write!(f, "<)>"),
            Comma => write!(f, "<,>"),
            Operator(ref op) => write!(f, "${}$", op),
            Ellision => write!(f, "<..>"),
            Name(ref name) => write!(f, "_{}_", name),
            Module => write!(f, "<module>"),
            Exposing => write!(f, "<exposing>"),
            Import => write!(f, "<import>"),
            As => write!(f, "<as>"),
            DocComment(_) => write!(f, "<{{-| ... -}}>"),
            LBrace => write!(f, "<{{>"),
            RBrace => write!(f, "<}}>"),
            LBracket => write!(f, "<[>"),
            RBracket => write!(f, "<]>"),
            Lambda => write!(f, "<λ>"),
            RArrow => write!(f, "<->>"),
            Case => write!(f, "<case>"),
            Of => write!(f, "<of>"),
            Underscore => write!(f, "<_>"),
            If => write!(f, "<if>"),
            Then => write!(f, "<then>"),
            Else => write!(f, "<else>"),
            Pipe => write!(f, "<|>"),
            Assign => write!(f, "<=>"),
            TypeDeclr => write!(f, "<:>"),
            Type => write!(f, "<type>"),
            Alias => write!(f, "<alias>"),
            Infixr => write!(f, "<infixr>"),
            Infixl => write!(f, "<infixl>"),
            Port => write!(f, "<port>"),
            Where => write!(f, "<where>"),
            Let => write!(f, "<let>"),
            In => write!(f, "<in>"),
            StringLit(_) => write!(f, "\"..\""),
            Number(ref content) => write!(f, "#{}#", content),
            Char(ref content) => write!(f, "'{}'", content),
            LetIndent => write!(f, "<LET INDENT>"),
            CaseIndent => write!(f, "<CASE INDENT>"),
            Endcase => write!(f,"<ESAC>"),
        }
    }
}

#[derive(Debug,PartialEq,Eq)]
pub enum LexError { }


