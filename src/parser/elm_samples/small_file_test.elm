module ModuleName exposing (g,f,Type1(..))

{-| module doc -}

import Http
import Http.Error as HE

type Type1 = Alt1 | Alt2

{-| function doc -}
g x =
    case expr_1 of
        Ptrn_1 ->
            let
                ident_1 = expr_2
            in
                case expr_2 of
                    Ptrn_2 -> expr_3
                    Ptrn_3 -> expr_4
        Ptrn_4 ->
            expr_5

f x =
 let
     expr_1 =
         fun
         (let ident_2 = expr_12 in expr_13)
         (expr_14)
     expr_19 = expr_20 |> expr_21 expr_22
 in
     ( expr_23, expr_24 )

-- Some comments
h : T1 T2 -> T3 T4 -> T5 -> ( T6, T7 T9 )
h  ptrn_6 =
    let
        ident_3 = expr_26
    in
        expr_40

type alias Type2 = { field1 : String, field2 : Int }

