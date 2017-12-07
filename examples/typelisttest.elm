module Typelisttest exposing (..)

type alias R1 v1 =
    { f1 : T1 -> H1 T2 T3 -> H2 v2 -> H3 v3 -> H4 v4 v5
    , f2 : (T4, H5 T5 T6, T7, { v6 | f3 : T8 })
    , f4 : ( ((T9)), ())
    }


type T10 v7 v8 v9
    = C1 { v10 | f5: T11, f6: T12 }
    | C2 { f7: T13 T14, f8: { f9 : T15, f10: T16 } } { f11: (T17, T18) }
    | C3 {} () (T19 T20)
    | C4 (T21 -> T22 -> (T23 -> T24) -> T25) (T26 -> (T27 -> T28))
