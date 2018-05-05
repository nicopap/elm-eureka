use super::*;


macro_rules! hash_map {
    ($interner:expr ; $($from:expr => $to:expr),*) => ({
        let mut base_map = FxHashMap::default();
        $(
            let from = $interner.get_or_intern_path($from);
            let to = $interner.get_or_intern_path($to);
            base_map.insert(from, to) ;
        )*
        base_map
    });
}

macro_rules! make_env {
    ($(($from:expr => $($to:expr),*)),*) => ({
        let mut interner = Interner::new();
        let mut map = FxHashMap::default();
        $(
            let int_name = interner.get_or_intern_path($from);
            let target = make_env!(@ target interner, $($to),*);
            map.insert(int_name, target);
        )*
        (interner, map)
    });
    (@ target $interner:expr, $($name:expr),* ) => ({
        let mut names = FxHashSet::default();
        $(
            let int_name = $interner.get_or_intern_name($name);
            names.insert(int_name);
        )*
        names
    });
}

const ELM_FILE : &'static str = r##"

import A.Proper.Path as App
import B.Other as Bo exposing (banana, Britain)
import D.Other.Path
import C exposing (..)

main = main
"##;


#[test]
fn test_import_env() {
    let (mut interner, environment) = make_env!(
        ("A.Proper.Path" => "apple", "ananas", "Austria"),
        ("B.Other"       => "banana", "boat", "Britain"),
        ("C"             => "condor", "cucumber", "Corea"),
        ("D.Other.Path"  => "Dominique", "durian", "dolphin"),
        ("E"             => "escargot", "elephant", "Equador")
    );

    let owned_file = ELM_FILE.to_owned();
    let mut parser = Parser::new(owned_file.chars());

    let imports = (*parser.imports()).to_owned();
    let result = import_env(
        &mut interner,
        &environment,
        imports.iter()
    ).unwrap();
    let expected = hash_map!( interner;
        // import A.Proper.Path as App
        "App.apple"   => "A.Proper.Path.apple",
        "App.ananas"  => "A.Proper.Path.ananas",
        "App.Austria" => "A.Proper.Path.Austria",
        // import B.Other as Bo exposing (banana, Britain)
        "Bo.boat"    => "B.Other.boat",
        "Bo.banana"  => "B.Other.banana",
        "Bo.Britain" => "B.Other.Britain",
        "banana"     => "B.Other.banana",
        "Britain"    => "B.Other.Britain",
        // import D.Other.Path
        "D.Other.Path.Dominique" => "D.Other.Path.Dominique",
        "D.Other.Path.durian"    => "D.Other.Path.durian",
        "D.Other.Path.dolphin"   => "D.Other.Path.dolphin",
        // import C exposing (..)
        "condor" => "C.condor",
        "cucumber" => "C.cucumber",
        "Corea" => "C.Corea",
        "C.condor" => "C.condor",
        "C.cucumber" => "C.cucumber",
        "C.Corea" => "C.Corea"
    );
    macro_rules! solve {
        ($val:expr)  => (interner.resolve_path($val).unwrap())
    }
    let text_result : FxHashMap<String,String> =
        result.clone().into_iter()
            .map(|(ref k,ref v)| (solve!(k),solve!(v)))
            .collect();
    let text_expected : FxHashMap<String,String> =
        expected.clone().into_iter()
            .map(|(ref k,ref v)| (solve!(k),solve!(v)))
            .collect();

    assert_eq!(text_result, text_expected);
    assert_eq!(result, expected);

}
