import Html exposing (li, text, ul)
import Html.Attributes exposing (class, Stuff(..))


{-| This {- Hello :)-}

Et maintenant le voyage au supermarché!
-}
main =
  ul [class "grocery-list"]
    [ li [] [.model >> text "Pamplemousse"]
    , li [] [Name.Wow << text "Ananas"]
    , li [] [text 103 "Jus d'orange"]
    , li [] [text "Bœuf"]
    ]


-- Thanks to "Flight of the Conchords" for creating "Foux Du Fafa"
