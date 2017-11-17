#![feature(io)] extern crate elm_eureka;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

use elm_eureka::lexer::Lexer;

pub fn main() {
    let file = File::open("examples/elmjutsu-5k.elm").unwrap();
    let reader = BufReader::new(file);
    let lex = Lexer::new(reader.chars().map( |x| x.unwrap() ));

    for token in lex {
        print!("{} ", token);
    }
}
