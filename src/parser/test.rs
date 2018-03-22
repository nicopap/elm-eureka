#![cfg(test)]

use test::{Bencher,black_box};

use super::Parser;
use super::tree::ElmModule;


// Should succesfully compile the compilation of examples available in
// small_indent_test.elm
#[test] fn small_indent_test_parses() {
    let source_txt = include_str!("../../examples/small_indent_test.elm").to_owned();
    let _parser = Parser::new(source_txt.chars()).into_parse_tree();
}

// Should succesfully compile `small_file_test.elm`, tests approximatively
// if the file was correctly parsed.
#[test] fn small_file_test_validity() {
    let source_txt = include_str!("../../examples/small_file_test.elm").to_owned();
    let ElmModule {name, doc, imports, types, functions,
        infixities, ports, ..} = Parser::new(source_txt.chars()).into_parse_tree();
    assert_eq!(name, "ModuleName".to_owned());
    assert_eq!(doc, Some(" module doc ".to_owned()));
    assert_eq!(imports.len(), 2);
    assert_eq!(types.len(), 2);
    assert_eq!(functions.len(), 3);
    assert_eq!(infixities.len(), 0);
    assert!(ports.is_none());
}

#[bench] fn elmjutsu_parsetime(bench : &mut Bencher) {
    let source_txt = include_str!("../../examples/elmjutsu-5k.elm");
    bench.iter(|| {
        let owned_txt = source_txt.to_owned();
        let parser = Parser::new(owned_txt.chars());
        black_box({parser.into_parse_tree();()})
    })
}
