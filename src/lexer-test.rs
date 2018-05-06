use super::*;
use test::{Bencher,black_box};

macro_rules! s {
    ($fn_to_call:ident, $str_to_conv:expr) => (
        $fn_to_call(String::from($str_to_conv))
    )
}
const UNORDERED_LIST_EXAMPLE : &str = include_str!("parser/elm_samples/unordered_list.elm");
const HELLO_WORLD : &str = include_str!("parser/elm_samples/no_head.elm");

#[test] fn test_lexer() {
    use tokens::ElmToken::*;
    let str_input = String::from(UNORDERED_LIST_EXAMPLE);
    let lexer = str_input.chars().lex().map(|(_p,t)| t);
    let token_vec = lexer.collect::<Vec<_>>();
    assert_eq!(
        token_vec, vec![Import, s!(Name,"Html"), Exposing, LParens,
        s!(Name,"li"), Comma, s!(Name,"text"), Comma, s!(Name,"ul"),
        RParens, Newline(0), Import, s!(Name,"Html.Attributes"),
        Exposing, LParens, s!(Name,"class"), Comma, s!(Name, "Stuff"),
        LParens, Ellision, RParens, RParens, Newline(0),
        DocComment(r#" This {- Hello :)-}

Et maintenant le voyage au supermarché!
"#.to_owned()), Newline(0), s!(Name,"main"), Assign,
        Newline(2), s!(Name,"ul"), LBracket, s!(Name,"class"), s!(StringLit,"grocery-list"), RBracket,
        Newline(4),LBracket,s!(Name,"li"),LBracket, RBracket,
        LBracket, s!(Name, ".model"), s!(Operator, ">>"),
        s!(Name,"text"), s!(StringLit,"Pamplemousse"), RBracket,
        Newline(4), Comma, s!(Name,"li"), LBracket, RBracket,
        LBracket, s!(Name, "Name.Wow"), s!(Operator, "<<"),
        s!(Name,"text"), s!(StringLit,"Ananas"), RBracket,
        Newline(4), Comma, s!(Name,"li"), LBracket, RBracket,
        LBracket, s!(Name,"text"), s!(Number, "103"), s!(StringLit,"Jus d'orange"), RBracket,
        Newline(4), Comma, s!(Name,"li"), LBracket, RBracket,
        LBracket, s!(Name,"text"), s!(StringLit,"Bœuf"), RBracket,
        Newline(4), RBracket, Newline(0)]);
}

#[test] fn test_hello_lex_location_tracking() {
    use tokens::ElmToken::*;
    let str_input = String::from(HELLO_WORLD);
    let lexer = str_input.chars().lex().map(|((p,q),t)|
        (p.0, p.1, q.0, q.1, t)
    );
    let token_vec = lexer.collect::<Vec<_>>();
    assert_eq!(token_vec, vec![
        (1 ,1 ,1 ,7 ,Import),           (1,8,1,12,s!(Name,"Html")),
        (1 ,13,1 ,21,Exposing),         (1 ,22,1 ,23,LParens),
        (1 ,23,1 ,27,s!(Name,"text")),  (1 ,27,1 ,28,RParens),
        (3 ,1 ,3 ,1 ,Newline(0)),
        (3 ,1 ,6 ,3 ,s!(DocComment,"Moduledoc\n\n\n")),
        (8 ,1 ,8 ,1 ,Newline(0)),
        (8 ,1 ,8 ,13,s!(DocComment,"Fundoc ")),
        (9 ,1 ,9 ,1 , Newline(0)), (9 ,1 ,9 ,5 ,s!(Name,"main")),
        (9 ,15,9 ,16,Assign),      (10,3 ,10,3 , Newline(2)),
        (10,3 ,10,7 ,s!(Name,"text")),
        (10,8 ,10,23,s!(StringLit,"Hello, World!"))
    ]);
}

// IMPORTANT NOTE: This test in very limited, there is a lot of ways to
// invalidate the number lexer (notably `-0x499`), but this is not
// an issue until I decide it is.
#[test] fn number_lexing() {
    use tokens::ElmToken::*;
    let src = "name-10e-10 10-10 10e+99 0x1e+99 0xabcd --0x449".to_owned();
    let tknzd = src.chars().lex().map(|(_p,t)| t).collect::<Vec<_>>();
    assert_eq!(tknzd, vec![
        s!(Name,"name"), s!(Operator,"-"), s!(Number,"10e-10"),
        s!(Number,"10"), s!(Operator,"-"), s!(Number,"10"),
        s!(Number,"10e+99"), s!(Number,"0x1e"), s!(Operator,"+"),
        s!(Number,"99"), s!(Number,"0xabcd") ]);
}

// This should be a proper test, but it fails
// miserably :\ For the +--0x.. even the standard
// implementation chokes.
fn operator_lexing() {
    use tokens::ElmToken::*;
    let src = "0.-1+.2 3.a-.b+--0x449".to_owned();
    let tknzd = src.chars().lex().map(|(_p,t)| t).collect::<Vec<_>>();
    assert_eq!(tknzd, vec![
        s!(Number,"0"), s!(Operator,".-"), s!(Number,"1"),
        s!(Operator,"+."), s!(Number,"2"),
        s!(Number,"3"), s!(Name,".a"), s!(Operator,"-."),
        s!(Name,"b"), s!(Operator,"+") ]);
}

macro_rules! consumer_test {
    ($input_value:expr, $expected_result:expr, $input_remain:expr, $to_test:ident) => (
        let mut input = $input_value.chars().peekable();
        let head =input.next().unwrap();
        let (_, token) = $to_test(&mut input, head);
        assert_eq!(token, $expected_result);
        assert_eq!(input.collect::<String>(), $input_remain);
    );
    (S, $input_value:expr, $expected_result:expr, $input_remain:expr, $to_test:ident) => (
        let mut input = $input_value.chars().peekable();
        input.next().unwrap();
        let (_, token) = $to_test(&mut input);
        assert_eq!(token, $expected_result);
        assert_eq!(input.collect::<String>(), $input_remain);
    )
}

#[test] fn test_consume_operator() {
    use tokens::ElmToken::Operator;
    consumer_test!( "+*--12", s!(Operator,"+*--"), "12", consume_operator);
    consumer_test!( "<*| wow |*>", s!(Operator,"<*|"), " wow |*>", consume_operator);
}
#[test] fn test_consume_char() {
    use tokens::ElmToken::Char;
    consumer_test!(S, r#"'\xfff' 10"#, s!(Char,r#"\xfff"#), " 10", consume_char);
    consumer_test!(S, r#"'\\' ten"#, s!(Char,r#"\\"#), " ten", consume_char);
    consumer_test!(S, r#"'\''''"#, s!(Char,r#"\'"#), "''", consume_char);
}
#[test] fn test_consume_number() {
    use tokens::ElmToken::Number;
    consumer_test!( "0xabcd-10", s!(Number,"0xabcd"), "-10", consume_number);
    consumer_test!( "10e+34-10", s!(Number,"10e+34"), "-10", consume_number);
    consumer_test!( "3.1415-10", s!(Number,"3.1415"), "-10", consume_number);
}
#[test] fn test_consume_name() {
    use tokens::ElmToken::{Name,Type};
    consumer_test!( "Some.test-10", s!(Name,"Some.test"), "-10", consume_name);
    consumer_test!( "type -10", Type, " -10", consume_name);
    consumer_test!( "Some.test-10", s!(Name,"Some.test"), "-10", consume_name);
    consumer_test!( "Some.Test.hello", s!(Name,"Some.Test.hello"),"", consume_name);
    consumer_test!( ".hello", s!(Name,".hello"), "", consume_name);
    consumer_test!( "model.hello", s!(Name,"model.hello"), "", consume_name);
}

macro_rules! block_test_boilerplate {
    ($input_value:expr, $input_remain:expr, $buffer_res:expr) => (
        let mut input = $input_value.chars().peekable();
        let (_, content) = consume_block_comment(&mut input);
        let expected_output = Some(String::from($buffer_res));
        assert_eq!(content, expected_output);
        assert_eq!(input.collect::<String>(), $input_remain);
    );
    ($input_value:expr, $input_remain:expr) => (
        let mut input = $input_value.chars().peekable();
        let (_, content) = consume_block_comment(&mut input);
        let expected_output = None;
        assert_eq!(content, expected_output);
        assert_eq!(input.collect::<String>(), $input_remain);
    )
}
#[test] fn test_consume_block_comment1() {
    block_test_boilerplate!("--}float", "float");
}
#[test] fn test_consume_block_comment2() {
    block_test_boilerplate!("-hello world!!-}float", "float");
}
#[test] fn test_consume_block_comment3() {
    block_test_boilerplate!("-hello {-world!!-}float-}remains", "remains");
}
#[test] fn test_consume_block_comment4() {
    block_test_boilerplate!("-|-}float", "float", "");
}
#[test] fn test_consume_block_comment5() {
    block_test_boilerplate!(
        "-|hello {- hi-} world!!-}float",
        "float",
        "hello {- hi-} world!!"
    );
}
#[test] fn test_consume_block_comment6() {
    block_test_boilerplate!(
        "-|héllö {- hî-} wörld!ŵ\n-}ẑloat",
        "ẑloat",
        "héllö {- hî-} wörld!ŵ\n"
    );
}

#[bench] fn bench_lexer(bench: &mut Bencher) {
    let source_txt = include_str!("../examples/elmjutsu-5k.elm");
    bench.iter(|| {
        let owned_txt = source_txt.to_owned();
        let token_stream = owned_txt.chars().lex();
        black_box({token_stream.collect::<Vec<_>>();()})
    })
}
