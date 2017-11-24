module A exposing (..)

f x =
 let
     expr_1 =
         expr_2 (case expr_3 of
             Ok ptrn_1 ->
                 expr_4
             Err ptrn_2 ->
                 let
                     ident_1 =
                         case expr_5 of
                             Http.BadUrl ptrn_3 ->
                                 expr_6

                             Http.Timeout ->
                                 expr_7

                             Http.NetworkError ->
                                 expr_8

                             Http.BadStatus ptrn_4 ->
                                 expr_9

                             Http.BadPayload ptrn_5 ->
                                 expr_10
                 in
                    expr_11
         )
         (let ident_2 = expr_12 in expr_13)
         (expr_14)

     expr_15 =
         expr_16
             |> expr_17 expr_18

     expr_19 =
         expr_20
             |> expr_21 expr_22
 in
     ( expr_23, expr_24 )
