//! pretty-prints the parser tree for the elmjutsu-5k.elm file
#![feature(io)]

extern crate elm_eureka;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::env::args;

use elm_eureka::Parser;

pub fn main() {
    let file_to_read =
        args().nth(1).unwrap_or(String::from("examples/elmjutsu-5k.elm"));
    let file = File::open(file_to_read).unwrap();
    let reader = BufReader::new(file);
    let char_stream = reader.chars().map(|x| x.unwrap());
    let parse_tree = Parser::new(char_stream).into_parse_tree();
    println!("{:#?}", parse_tree);
}
