use super::*;

macro_rules! fresh_scoped_map {
    ($($from:expr => $to:expr),*) => ({
        let mut base_map = FxHashMap::default();
        {$( base_map.insert($from, $to) ;)*}
        ScopedMap::new(base_map)
    });
}
macro_rules! insert_scope {
    ($to_extend:expr, $($from:expr => $to:expr),*) => ({
        $( $to_extend.insert($from, $to) ;)*
    });
    ($to_extend:expr, $($key:expr),*) => ({
        $( $to_extend.insert($key).unwrap() ;)*
    });
}

macro_rules! assert_equal_in {
    ($map:expr , $($key:expr => $value:expr),*) => ({
        $( assert_eq!($map.get(&$key), $value) );*
    });
}

#[test]
fn test_scoped_map_archaic() {
    let mut scoped_map = fresh_scoped_map!(
        "a" => "asparagus",
        "b" => "banana",
        "c" => "cucumber",
        "e" => "elephant",
        "f" => "fox",
        "g" => "garlic"
    );
    scoped_map.enter_scope();
    insert_scope!(scoped_map, "g"=>"glasgow", "h"=>"hedgehog");

    scoped_map.enter_scope();
    insert_scope!(scoped_map, "g"=>"goose", "b"=>"bank", "c"=>"chocolate");

    assert_equal_in!(scoped_map,
        "g" => Some(&"goose"),
        "b" => Some(&"bank"),
        "h" => Some(&"hedgehog"),
        "d" => None
    );

    scoped_map.leave_scope();
    assert_equal_in!(scoped_map,
        "g" => Some(&"glasgow"),
        "e" => Some(&"elephant"),
        "c" => Some(&"cucumber")
    );
}

#[test]
fn test_scoped_map_within() {
    let scope = fresh_scoped_map!(
        "a" => "asparagus",
        "b" => "banana",
        "c" => "cucumber",
        "e" => "elephant",
        "f" => "fox",
        "g" => "garlic"
    );

    let scope = scope.within_scope(|mut scope| {
        insert_scope!(scope, "g"=>"glasgow", "h"=>"hedgehog");

        assert_equal_in!(scope,
            "h" => Some(&"hedgehog"),
            "f" => Some(&"fox")
        );
        let scope = scope.within_scope(|mut scope| {
            insert_scope!(scope,"g"=>"goose","b"=>"bank","c"=>"chocolate");

            assert_equal_in!(scope,
                "h" => Some(&"hedgehog"),
                "a" => Some(&"asparagus"),
                "g" => Some(&"goose"),
                "b" => Some(&"bank")
            );
            scope
        });
        scope
    });
    assert_equal_in!(scope,
        "g" => Some(&"garlic"),
        "h" => None,
        "c" => Some(&"cucumber")
    );
}

#[test]
fn test_scoped_map_is_in_last() {
    let scope = fresh_scoped_map!(
        "a" => "asparagus",
        "b" => "banana",
        "c" => "cucumber",
        "e" => "elephant",
        "f" => "fox",
        "g" => "garlic"
    );

    let scope = scope.within_scope(|mut scope| {
        insert_scope!(scope, "g"=>"glasgow", "h"=>"hedgehog");

        let scope = scope.within_scope(|mut scope| {
            insert_scope!(scope,"g"=>"goose","b"=>"bank","c"=>"chocolate");

            assert!(scope.is_in_last_scope(&"b"));
            assert!( ! scope.is_in_last_scope(&"h"));
            assert!( ! scope.is_in_last_scope(&"a"));
            assert!( ! scope.is_in_last_scope(&"k"));
            scope
        });
        scope
    });
    assert!(scope.is_in_last_scope(&"b"));
    assert!(scope.is_in_last_scope(&"a"));
    assert!( ! scope.is_in_last_scope(&"h"));
    assert!( ! scope.is_in_last_scope(&"k"));
}

#[test]
fn test_namespace() {
    let mut ctx = NameSpace::new(Vec::new());
    insert_scope!(ctx, "Base", "List", "Maybe");
    assert_eq!(ctx.insert("Base"), Err(errors::Scope::AlreadyDefined));

    let ctx = ctx.within_scope("SecondBase", |mut ctx| {
        insert_scope!(ctx, "Result", "List");

        assert_eq!(ctx.insert("Base"), Ok(()));
        let guess1 : &[&str] = &["SecondBase", "Base"];
        let guess2 : &[&str] = &["SecondBase", "List"];
        let guess3 : &[&str] = &["Maybe"];
        let guess4 : &[&str] = &["SecondBase", "Result"];
        assert_equal_in!(ctx,
            &vec!["Base"]   => Ok(guess1),
            &vec!["List"]   => Ok(guess2),
            &vec!["Maybe"]  => Ok(guess3),
            &vec!["Result"] => Ok(guess4),
            &vec!["Process"] => Err(errors::Scope::FreeVariable)
        );
        ctx
    });
}
