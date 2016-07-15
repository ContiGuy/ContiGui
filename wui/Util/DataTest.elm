module DataTest exposing (..)

import Html              exposing (..)
import Json.Decode       exposing ((:=)) -- as JD  -- exposing (..)  --  (decodeString, (:=), object1, object2, string, int, list)
import Json.Decode.Extra -- exposing (withDefault, lazy)

type alias Widget =
  { id       : Id
  --, label    : String
  --, descr    : String
  --, value    : Value
  --, fmtr     : Formatter
  }

type alias Id = String

decodeWidget : Json.Decode.Decoder Widget
decodeWidget =
  Json.Decode.object1 Widget
    --( withDefault -33 ( "id" := int ) )
    ( "id" := Json.Decode.string )

type WidgetTree
    = WidgetLeaf
    | WidgetNode Widget ( List WidgetTree )

{-| Json Decoder for a recursive data structure
-}
decodeWidgetTree : Json.Decode.Decoder WidgetTree
decodeWidgetTree =
  let
    treeList =
      Json.Decode.list decodeWidgetTree

    lazyTree_WORKS =
      Json.Decode.Extra.lazy
        (\_ -> (Json.Decode.list decodeWidgetTree))

    lazyTree_HANGS =
      Json.Decode.Extra.lazy
        (\_ -> treeList)
  in
  Json.Decode.object2 WidgetNode
    decodeWidget
    ( Json.Decode.Extra.withDefault [] (
--      "kids" := lazyTree_WORKS ) )
      "kids" := lazyTree_HANGS ) )
      
        -- Json.Decode.Extra.lazy
                --(\_ -> (Json.Decode.list decodeWidgetTree)) ) )



-- VIEW

decodeStr2LI : Json.Decode.Decoder a -> String -> Html b
decodeStr2LI decoder s =
    li [] [
      s ++ "  >>>  " ++ (Json.Decode.decodeString decoder s |> toString)
      |> text
    ]

decodeTree2Text2LI : String -> Html a
decodeTree2Text2LI s =
    decodeStr2LI decodeWidgetTree s

decodeWidget2Text2LI : String -> Html a
decodeWidget2Text2LI s =
    decodeStr2LI decodeWidget s

recordsLI : Html a
recordsLI =
  li [] [ text "Widgets:", ul [] [
      decodeWidget2Text2LI """{"id":3, "tree":{"name": "joe"}}"""
  ] ]

treesLI : Html a
treesLI =
  li [] [ text "Trees:", ul [] [
      text "OK:"
    , decodeTree2Text2LI """{"id":"x1", "kids": [{"id":"y1", "kids":[]},{"id":"y2", "kids":[{"id":"z1", "kids":[]}]}]}"""
    , decodeTree2Text2LI """{"id":"x2", "name": "joe", "kids":[]}"""
    , decodeTree2Text2LI """{"id":"x2", "name": "joe"}"""
    , text "FAIL:"
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI """{"name": "joe", "kids": {"name":"jim"}}"""
    , decodeTree2Text2LI """{"kids": [{"kids":[]}]}"""
    , decodeTree2Text2LI """{"name": "joe"}"""
    ] ]


main : Html a
main =
    ul [] [
      treesLI
    , recordsLI
    ]
