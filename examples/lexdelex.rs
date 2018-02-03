//! Pretty-prints the tokens created from lexing the elmjutsu-5k.elm file
#![feature(io)]

extern crate elm_eureka;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::env::args;

use elm_eureka::lexer::LexableIterator;

pub fn main() {
    let file_to_read =
        args().nth(1).unwrap_or(String::from("examples/elmjutsu-5k.elm"));
    let file = File::open(file_to_read).unwrap();
    let reader = BufReader::new(file);
    let lex = reader.chars().map( |x| x.unwrap() ).lex().map(|(_p,t)| t);

    for token in lex {
        print!("{} ", token);
    }
}
