#![cfg(test)]

use test::{Bencher,black_box};

use super::Parser;


// Should succesfully compile the compilation of examples available in
// small_indent_test.elm
#[test] fn small_indent_test() {
    let source_txt = include_str!("../../examples/small_indent_test.elm").to_owned();
    let _parser = Parser::new(source_txt.chars()).into_parse_tree();
}


#[bench] fn elmjutsu_parsetime(bench : &mut Bencher) {
    let source_txt = include_str!("../../examples/elmjutsu-5k.elm");
    bench.iter(|| {
        let owned_txt = source_txt.to_owned();
        let parser = Parser::new(owned_txt.chars());
        black_box({parser.into_parse_tree();()})
    })
}
