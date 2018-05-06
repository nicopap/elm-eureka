use super::*;
use std::fmt::{self, Display};

macro_rules! conca {
    (direct $to:expr; $f:expr; $iter:expr; $($separ:ident => $lit:expr),* ) => (
        match $to {
            $( Separator::$separ => {
                for value in $iter { write!($f, $lit, value)?; }
            }),*
        });
    (open $to:expr; $f:expr; $iter:expr; $($separ:ident => $lit:expr),* ) => (
        match $to {
            $( Separator::$separ => {
                for &(_, ref value) in $iter { write!($f, $lit, value)?; }
            }),*
        });
    (default $which:ident $to:expr; $f:expr; $iter:expr) => (
        conca!($which $to; $f; $iter;
            Commas     => ", {}",
            Spaces     => " {}",
            Cons       => " :: {}",
            Arrow      => " -> {}"
        )
    )
}


enum Separator { Commas, Spaces, Cons, Arrow, }

struct ShowTpl<'a, X:'a,T:'a>(Separator, &'a Vec<(X,T)>);
impl<'a, X:'a,T:Display+'a> Display for ShowTpl<'a, X,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value_iter = self.1.iter();
        if let Some(&(_,ref value)) = value_iter.next() {
            write!(f, "{}", value)?;
        } else { return write!(f, "") }
        conca!(default open self.0; f; value_iter);
        Ok(())
    }
}

struct Show<'a, T:'a>(Separator, &'a Vec<T>);
impl<'a, T:Display+'a> Display for Show<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value_iter = self.1.iter();
        if let Some(value) = value_iter.next() {
            write!(f, "{}", value)?;
        } else { return write!(f, "") }
        conca!(default direct self.0; f; value_iter);
        Ok(())
    }
}

struct ShowFldRe<'a, N:'a, X:'a,T:'a>(&'a Vec<(N,(X,T))>);
impl<'a, N: Display+'a, X:'a,T:Display+'a> Display
for ShowFldRe<'a, N,X,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value_iter = self.0.iter();
        if let Some(&(ref name, (_, ref value))) = value_iter.next() {
            write!(f, "{} = {}", name, value)?;
        } else { return write!(f, "") }
        for &(ref name, (_, ref value)) in value_iter {
            write!(f, ", {} = {}", name, value)?;
        }
        Ok(())
    }
}

struct ShowFldRt<'a, N:'a, X:'a,T:'a>(&'a Vec<(N,(X,T))>);
impl<'a, N: Display+'a, X:'a,T:Display+'a> Display
for ShowFldRt<'a, N,X,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value_iter = self.0.iter();
        if let Some(&(ref name, (_, ref value))) = value_iter.next() {
            write!(f, "{}: {}", name, value)?;
        } else { return write!(f, "") }
        for &(ref name, (_, ref value)) in value_iter {
            write!(f, ", {}: {}", name, value)?;
        }
        Ok(())
    }
}

struct ShowFldPx<'a, N:'a, X:'a,T:'a>(&'a Vec<((X,T),N)>);
impl<'a, N: Display+'a, X:'a,T:Display+'a> Display
for ShowFldPx<'a, N,X,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &((_, ref exp), ref op) in self.0.iter() {
            write!(f, "{} {} ", exp, op)?;
        };
        Ok(())
    }
}

struct ShowFldCs<'a, N:'a, X:'a,T:'a>(&'a Vec<((X,T),(X,N))>);
impl<'a, N: Display+'a, X:'a,T:Display+'a> Display
for ShowFldCs<'a, N,X,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut value_iter = self.0.iter();
        if let Some(&((_, ref pat), (_, ref expr))) = value_iter.next() {
            write!(f, "{} -> {}", pat, expr)?;
        } else { return write!(f, "") }
        for &((_, ref pat), (_, ref expr)) in value_iter {
            write!(f, "; {} -> {}", pat, expr)?;
        };
        Ok(())
    }
}

impl<name: Display, T> Display for Pattern_<name,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Pattern_::*; match *self {
        ArgConstructor(ref name, ref patterns) =>{
            let show = ShowTpl(Separator::Spaces, &patterns);
            write!(f, "({} {})", name, show)?;
        },
        Record(ref fields) =>{
            let show = Show(Separator::Commas, &fields);
            write!(f, "{{ {} }}", show)?;
        },
        AliasBind(ref pattern, ref name) => {
            let &(_, ref pattern) = pattern.as_ref();
            write!(f, "({} as {})", pattern, name)?;
        },
        Tuple(ref patterns) =>{
            let show = ShowTpl(Separator::Commas, &patterns);
            write!(f, "({})", show)?;
        },
        Bind(ref name) | Constructor(ref name) =>
            write!(f, "{}", name)?,

        StringLit(ref name)| Character(ref name)| Number(ref name) =>
            write!(f, "{}", name)?,

        UnitType  => write!(f, "()")?,
        Discard   => write!(f, "_")?,
        EmptyList => write!(f, "[]")?,
        List(ref patterns) =>{
            let show = ShowTpl(Separator::Commas, &patterns);
            write!(f, "[{}]", show)?;
        },
        Decons(ref patterns) =>{
            let show = ShowTpl(Separator::Cons, &patterns);
            write!(f, "({})", show)?;
        },
        };
        Ok(())
    }
}

impl<name: Display> Display for Type_<name> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type_::*; match *self {
        Terminal(ref name) | Variable(ref name) =>
            write!(f, "{}", name)?,

        Tuple(ref types) => {
            let show = ShowTpl(Separator::Commas, &types);
            write!(f, "({})", show)?;
        },
        Record { ref variable_over, ref fields } => {
            let show = ShowFldRt(&fields);
            if let &Some(ref over) = variable_over {
                write!(f, "{{ {} | {} }}", over, show)?;
            } else {
                write!(f, "{{ {} }}", show)?;
            }
        },
        Function(ref types) => {
            let show = ShowTpl(Separator::Arrow, &types);
            write!(f, "({})", show)?;
        },
        Application(ref name, ref types) => {
            let show = ShowTpl(Separator::Spaces, &types);
            write!(f, "({} {})", name, show)?;
        },
        EmptyRecord => write!(f, "{{ }}")?,
        UnitType => write!(f, "()")?,
        };
        Ok(())
    }
}

impl<name: Display,T> Display for Expression_<name,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expression_::*; match *self {
        Record { ref updates, ref fields, } =>{
            let show = ShowFldRe(fields);
            if let &Some(ref over) = updates {
                write!(f, "{{ {} | {} }}", over, show)?;
            } else {
                write!(f, "{{ {} }}", show)?;
            }
        },
        List(ref expressions) =>{
            let show = ShowTpl(Separator::Commas, expressions);
            write!(f, "[{}]", show)?;
        },
        Tuple(ref expressions) =>{
            let show = ShowTpl(Separator::Commas, expressions);
            write!(f, "({})", show)?;
        },
        Literal(ref literal, ref name) => match literal {
            &super::Literal::Char =>
                write!(f, "'{}'", name)?,
            &super::Literal::StringL =>
                write!(f, "\"{}\"", name)?,
            &super::Literal::Number =>
                write!(f, "{}", name)?,
        },
        IfThenElse { ref condition, ref then_branch, ref else_branch, } =>{
            write!(f, "if {} then {} else {}",
                   condition.as_ref().1,
                   then_branch.as_ref().1,
                   else_branch.as_ref().1,
            )?;
        },
        LetIn { ref declarations, ref expression, } =>{
            let mut declrs = declarations.iter();
            write!(f, "let {}", declrs.next().unwrap())?;
            for declr in declrs {
                write!(f, "; {}", declr)?;
            }
            write!(f, " in {}", expression.as_ref().1)?;
        },
        CaseOf { ref condition, ref branches, } =>{
            let show = ShowFldCs(branches);
            write!(f, "case {} of {}", condition.as_ref().1, show)?;
        },
        Lambda { ref arguments, ref body, } =>{
            let show = ShowTpl(Separator::Spaces, arguments);
            write!(f, "(\\{} -> {})", show, body.as_ref().1)?;
        },
        InfixApplication { ref prefixes, ref trailing, } =>{
            let show = ShowFldPx(prefixes);
            write!(f, "{}{}", show, trailing.as_ref().1)?;
        },
        Application(ref expressions) =>{
            let show = ShowTpl(Separator::Spaces, expressions);
            write!(f, "({})", show)?;
        },
        Variable(ref name) =>
            write!(f, "{}", name)?,
        PrefixOperator(ref name) =>
            write!(f, "({})", name)?,
        TupleConstructor(arr) =>{
            let commas = match arr {
                1 => ",", 2 => ",,", 3 => ",,,",
                4 => ",,,,", 5 => ",,,,,", 6 => ",,,,,,",
                7 => ",,,,,,,", _ => panic!("Too big of a tuple constructor \
                                            to display"),
            };
            write!(f, "({})", commas)?;
        },
        };
        Ok(())
    }
}

impl<name: Display,T> Display for LetDeclaration<name,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((_, ref type_)) = self.annotation {
            write!(f, "{} : {};", self.name.as_ref().unwrap(), type_)?;
        }
        if let Some(ref name) = self.name {
            write!(f, "{} ", name)?;
        }
        let show = ShowTpl(Separator::Spaces, &self.arguments);
        write!(f, "{} = {}", show, &self.body.1)
    }
}

impl<name: Display, T> Display for Function<name,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref doc) = self.doc {
            write!(f, "{{-| {}\n-}}\n", doc)?;
        }
        if let Some((_, ref type_)) = self.annotation {
            write!(f, "{} : {}\n", self.name, type_)?;
        }
        match self.kind {
            FunctionKind::Operator =>
                write!(f, "({}) ", self.name)?,

            FunctionKind::Regular =>
                write!(f, "{} ", self.name)?,
        };
        let show = ShowTpl(Separator::Spaces, &self.arguments);
        write!(f, "{} = {}\n", show, &self.body.1)
    }
}
