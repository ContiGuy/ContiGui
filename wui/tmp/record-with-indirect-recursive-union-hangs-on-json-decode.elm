module JsonTest exposing (..)

import Html              exposing (..)
import Json.Decode       exposing (..)
import Json.Decode.Extra exposing (withDefault, lazy)

type alias Record =
  { id : Int
  , tree : Tree
  }

emptyRec : Record
emptyRec =
  Record 0 Leaf

decodeRecord : Decoder Record
decodeRecord =
  object2 Record
    ( "id" := int )
    (withDefault Leaf decodeTree )
    

type Tree
    = Leaf
    | Node ( List Record )

decodeTree : Json.Decode.Decoder Tree
decodeTree =
  object1
    Node
        ("tree" := lazy (\_ ->
           (list decodeRecord)))

main : Html a
main =
  let
    jsonText = """{"id": 5, "tree": [{"id": 7, "tree": [{"id": 9}]}]}"""
  in
    decodeString decodeRecord jsonText |> toString |> text




























{-----------------------------------------------------------------------
------------------------------------------------------------------------
-----      extra      --------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------


decodeStr2LI : Json.Decode.Decoder a -> String -> Html b
decodeStr2LI decoder s =
    li [] [
      s ++ "  >>>  " ++ (decodeString decoder s |> toString)
      |> text
    ]

decodeTree2Text2LI : String -> Html a
decodeTree2Text2LI s =
    decodeStr2LI decodeTree s

decodeRecord2Text2LI : String -> Html a
decodeRecord2Text2LI s =
    decodeStr2LI decodeRecord s


main : Html a
main =
    ul [] [
      decodeRecord2Text2LI """{"id":3, "tree":{"name": "joe"}}"""
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI """{"name": "joe", "kids":[]}"""
    , decodeTree2Text2LI """{"name": "joe", "kids": {"name":"jim"}}"""
    , decodeTree2Text2LI """{"name": "joe"}"""
    ]
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------}
