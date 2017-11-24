cargo run --example listtypes <(echo 'module B exposing (..)

type alias Record =
    { field_one : Either Int (List String)
    , field_two : ({},())
    }
')
