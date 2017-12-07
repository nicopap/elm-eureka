#![cfg(test)]

use std::io::prelude::*;
use std::fs::File;

use super::Parser;


// Should succesfully compile the compilation of examples available in
// small_indent_test.elm
#[test] fn small_indent_test() {
    let mut file = File::open("examples/small_indent_test.elm").unwrap();
    let mut source_txt : String = String::new();
    file.read_to_string(&mut source_txt).unwrap();
    let _parser = Parser::new(source_txt.chars()).into_parse_tree();
}

