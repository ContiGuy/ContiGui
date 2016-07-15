module JsonTest exposing (..)

import Html              exposing (..)
import Json.Decode       exposing (..)  --  (decodeString, (:=), object1, object2, string, int, list)
import Json.Decode.Extra exposing (withDefault, lazy)

type alias Record =
  { id : Int
  , tree : Tree
  }

emptyRec =
  Record 0 Leaf

decodeRecord =
  object2 Record
    ( "id" := int )
    (withDefault Leaf ( "tree" := decodeTree ) )
    

type Tree
    = Leaf
    | Node ( List Record )

{-| Json Decoder for a recursive data structure
-}
decodeTree : Json.Decode.Decoder Tree
decodeTree =
  object1
    Node
    -- ("name" := string)
    --(withDefault Leaf
        ("kids" := lazy (\_ -> -- map Node
           (list decodeRecord))) --)
        -- !! -- ("kids" := treeNode))

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
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    ]
