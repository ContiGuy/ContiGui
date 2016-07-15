module JsonTest exposing (..)

import Html              exposing (..)
import Json.Decode       exposing (decodeString, (:=), object2, string)
import Json.Decode.Extra exposing (withDefault, lazy)

type alias Record =
  { id : Int
  , tree : Tree
  }

type Tree
    = Leaf
    | Node String Tree

{-| Json Decoder for a recursive data structure
-}
treeNode : Json.Decode.Decoder Tree
treeNode =
  object2
    Node
    ("name" := string)
    (withDefault Leaf
        ("kids" := lazy (\_ -> treeNode)))
        -- !! -- ("kids" := treeNode))

decodeStr2LI : Json.Decode.Decoder a -> String -> Html b
decodeStr2LI decoder s =
    li [] [
      s ++ "  >>>  " ++ (decodeString decoder s |> toString)
      |> text
    ]

decodeTree2Text2LI : String -> Html a
decodeTree2Text2LI s =
    decodeStr2LI treeNode s
    {---------------------------------
    li [] [
      s ++ "  >>>  " ++ (decodeString treeNode s |> toString)
      |> text
    ]
    ---------------------------------}

main : Html a
main =
    ul [] [
      decodeTree2Text2LI "{}"
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI """{"name": "joe", "kids":[]}"""
    , decodeTree2Text2LI """{"name": "joe", "kids": {"name":"jim"}}"""
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    , decodeTree2Text2LI "{}"
    ]
