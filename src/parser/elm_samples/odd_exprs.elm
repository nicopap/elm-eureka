import Html exposing (text)

-- let-expression in `case`, case with exhaustive pattern
a = case let y = () in y of () -> "hello world"

-- Unexpected pattern kind in exhaustive pattern
b (_ as x) = x -- b 10 -> 10

-- if-expression where condition is an if-expression
c = if if 0 == 0 then 1 == 1 else 1 /= 1 then 100 else 3

-- Unnatural priority of operands (the |> is inside the λ-expression)
d : number -> List number
d = \v -> v * 4 |> List.repeat 4 --> \v -> List.repeat 4 (v * 4)

-- Parsing edge case (λ-expression as trailing of infix-expression)
e : List number -> List number
e = List.map <| \v -> v * 4 --> List.map (\v -> v * 4)

-- (interesting for typing & parsing) if-expression where branches
-- are λ-expressions, of different types (but one is a subset of the other)
f : comparable -> Float -> Float
f = \x -> if x > 0 then \v -> v * 10 else \v -> v / 10

{-
main =
  text
    <| toString (a) ++ " | "
    ++ toString (b ('e','l','m')) ++ " | "
    ++ toString (c) ++ " | "
    ++ toString (d 3) ++ " | "
    ++ toString (e [4,5,6]) ++ " | "
    ++ toString (f -3 5)
--> "hello world" | ('e','l','m') | 100 | [12,12,12,12] | [16,20,24] | 0.5
-}
