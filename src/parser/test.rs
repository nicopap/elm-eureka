#![cfg(test)]

use test::{Bencher,black_box};
use std::fmt::Write;
use fxhash::{FxHashSet,FxHashMap};

use super::Parser;
use super::tree::{Expression_ as Expr, Module, Literal, Function};

macro_rules! hash_set {
    [$($value:expr),*] => ({
        let mut ret = FxHashSet::default();
        $(ret.insert($value);)*
        ret
    })
}

// Should succesfully compile the compilation of examples available in
// small_indent_test.elm
#[test]
fn small_indent_test_parses() {
    let source_txt = include_str!("../../examples/small_indent_test.elm").to_owned();
    let _parser = Parser::new(source_txt.chars()).into_parse_tree();
}

// Should succesfully compile `small_file_test.elm`, tests approximatively
// if the file was correctly parsed.
#[test]
fn small_file_test_validity() {
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
#[test]
fn hello_world_test() {
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

#[test]
fn odd_expressions_test() {
    let source_txt = include_str!("elm_samples/odd_exprs.elm").to_owned();
    let Module {functions, ..} =
        Parser::new(source_txt.chars()).into_parse_tree();
    let mut internal_rep = String::new();
    for function in functions {
        write!(internal_rep, "{}\n", function).unwrap();
    }
    const EXPECTED_REP : &'static str =
r#"a  = case let y = () in y of () -> "hello world"

b (_ as x) = x

c  = if if 0 == 0 then 1 == 1 else 1 /= 1 then 100 else 3

d : (number -> (List number))
d  = (\v -> v * 4 |> (List.repeat 4))

e : ((List number) -> (List number))
e  = List.map <| (\v -> v * 4)

f : (comparable -> Float -> Float)
f  = (\x -> if x > 0 then (\v -> v * 10) else (\v -> v / 10))

"#;
    assert_eq!(
        format!("{}", internal_rep),
        EXPECTED_REP.to_owned(),
    );
}

#[test]
fn parser_exported_names() {
    let no_head = include_str!("elm_samples/unordered_list.elm").to_owned();
    let mut no_head_parser = Parser::new(no_head.chars());
    assert_eq!(no_head_parser.exports(), hash_set!["main"]);

    let all_constr = include_str!("elm_samples/small_file_test.elm").to_owned();
    let mut all_constr_parser = Parser::new(all_constr.chars());
    assert_eq!(all_constr_parser.exports(), hash_set!["g","f","Type1","Alt1","Alt2"]);
}

#[bench]
fn elmjutsu_parsetime(bench : &mut Bencher) {
    let source_txt = include_str!("../../examples/elmjutsu-5k.elm");
    bench.iter(|| {
        let owned_txt = source_txt.to_owned();
        let parser = Parser::new(owned_txt.chars());
        black_box({parser.into_parse_tree();()})
    })
}

