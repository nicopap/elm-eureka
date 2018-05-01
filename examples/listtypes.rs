//! Get a list of all the types in an elm source file and print them
//! on stdout as a convoluted sentence.
#![feature(io)]

extern crate elm_eureka;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::env::args;

use elm_eureka::Parser;
use elm_eureka::parser::tree::{self, Type, Type_};

trait Descriptible {
    fn describe(&self) -> String;
}

impl<T:Descriptible> Descriptible for [T] {
    fn describe(&self) -> String {
        match self.len() {
            0 => "nothing".to_owned(),
            1 => self[0].describe(),
            2 => format!("{} and {}", self[0].describe(), self[1].describe()),
            _ => {
                let (head, tail) = self.split_first().unwrap();
                format!("{}, {}", head.describe(), tail.describe())
            },
        }
    }
}

impl Descriptible for (String, Type<String>) {
    fn describe(&self) -> String {
        let &(ref name, ref type_) = self;
        format!("a field {} is {}", name, type_.describe())
    }
}

impl Descriptible for Type<String> {
    fn describe(&self) -> String {
        match self.1 {
            Type_::Terminal(ref name) =>
                format!("the type {}", name),
            Type_::Variable(ref name) =>
                format!("a type variable {}", name),
            Type_::Application(ref name,ref arguments) =>
                format!(
                    "the higher-kinded type {} to which are applied ({})",
                    name,
                    arguments.describe()
                ),
            Type_::UnitType => "the unit type".to_owned(),
            Type_::EmptyRecord => "an empty record".to_owned(),
            Type_::Function(ref content) => format!(
                "a function which arguments are ({})",
                content.describe()
            ),
            Type_::Record { ref variable_over, ref fields } => {
                let variable_descr =
                    match *variable_over {
                        Some(ref name) =>
                            format!("generic over {} and", name),
                        None =>
                            "".to_owned(),
                    };
                format!(
                    "a record {} with ({})",
                    variable_descr,
                    fields.describe()
                )
            },
            Type_::Tuple(ref contained) =>
                format!("a tuple containing ({})", contained.describe()),
        }
    }
}

pub fn main() {
    let file_to_read =
        args().nth(1).unwrap_or_else(|| "examples/elmjutsu-5k.elm".to_owned());
    let file = File::open(file_to_read).unwrap();
    let char_stream = BufReader::new(file).chars().map(|x|x.unwrap());
    let parser = Parser::new(char_stream);
    for &(_,(_,ref genre)) in &parser.into_parse_tree().types {
        match *genre {
            tree::TypeGenre_::Alias {ref type_, ref name, ..} => {
                println!("type alias {}, is:\n  {}", name, type_.describe());

            },
            tree::TypeGenre_::Full{ ref alternatives, ref name, ..} => {
                println!("type {}, is:", name);
                for &(_,ref type_) in alternatives.iter() {
                    println!("  {} of {}", type_.0, type_.1.describe());
                }
            },
        }
    }
}
