#![cfg(test)]

use test::{Bencher,black_box};

use super::Parser;
use super::tree::{Expression_ as Expr, Module, Literal};


// Should succesfully compile the compilation of examples available in
// small_indent_test.elm
#[test] fn small_indent_test_parses() {
    let source_txt = include_str!("../../examples/small_indent_test.elm").to_owned();
    let _parser = Parser::new(source_txt.chars()).into_parse_tree();
}

// Should succesfully compile `small_file_test.elm`, tests approximatively
// if the file was correctly parsed.
#[test] fn small_file_test_validity() {
    let source_txt = include_str!("elm_samples/small_file_test.elm").to_owned();
    let Module {name, doc, imports, types, functions,
        infixities, ports, ..} = Parser::new(source_txt.chars()).into_parse_tree();
    assert_eq!(name, Some("ModuleName".to_owned()));
    assert_eq!(doc, Some(" module doc ".to_owned()));
    assert_eq!(imports.len(), 2);
    assert_eq!(types.len(), 2);
    assert_eq!(functions.len(), 3);
    assert_eq!(infixities.len(), 0);
    assert!(ports.is_none());
}

// Should be able to parse the "hello world" example provided at elm-lang.org
#[test] fn hello_world_test() {
    let source_txt = include_str!("elm_samples/no_head.elm").to_owned();
    let Module {name, doc, mut imports, types, mut functions,
        infixities, ports, ..} = Parser::new(source_txt.chars()).into_parse_tree();
    let only_function = functions.pop().expect(
        "There should be a function in the hello world file");
    imports.pop().expect("There should be an import in the hello world file");
    assert_eq!(name, None);
    assert_eq!(doc, None);
    assert_eq!(functions.len(), 0);
    assert_eq!(imports.len(), 0);
    assert_eq!(types.len(), 0);
    assert_eq!(&only_function.name, "main");

    if let (_,Expr::Application(mut only_function_app)) = only_function.body {
        if let Some((_,Expr::Literal(Literal::StringL, hello_str))) = only_function_app.pop() {
            if let Some((_,Expr::Variable(fn_name))) = only_function_app.pop() {
                assert_eq!((fn_name.as_ref(),hello_str.as_ref()), ("text", "Hello, World!"))
            } else {
                panic!("The first element of the application wasn't a simple \
                variable")
            }
        } else {
            panic!("The second element of the application wasn't a string \
            literal")
        }
    } else {
        panic!("the main body wasn't an application!")
    }
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
