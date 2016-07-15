module DataTest exposing (..)

import Html              exposing (..)
import Json.Decode       exposing ((:=))
import Json.Decode.Extra

type alias Record =
  { id : String
  }

decodeRecord : Json.Decode.Decoder Record
decodeRecord =
  Json.Decode.object1 Record
    ( "id" := Json.Decode.string )

type RecordTree
    = RecordLeaf
    | RecordNode Record ( List RecordTree )


decodeRecordTree : Json.Decode.Decoder RecordTree
decodeRecordTree =
  let
    treeList =
      Json.Decode.list decodeRecordTree

    lazyTree_WORKS =
      Json.Decode.Extra.lazy
        (\_ -> (Json.Decode.list decodeRecordTree))

    lazyTree_HANGS =
      Json.Decode.Extra.lazy
        (\_ -> treeList)
  in
  Json.Decode.object2 RecordNode
    decodeRecord
    ( Json.Decode.Extra.withDefault [] (
--      "kids" := lazyTree_WORKS ) )
      "kids" := lazyTree_HANGS ) )
      
        -- Json.Decode.Extra.lazy
                --(\_ -> (Json.Decode.list decodeRecordTree)) ) )



-- VIEW

main : Html a
main =
  let
    jsonTxt = """{"id":"x", "kids":[{"id":"y1", "kids":[]}]}"""
    -- works: jsonTxt = """{"id":"x"}"""
  in
    Json.Decode.decodeString decodeRecordTree jsonTxt |> toString |> text 
















{-----------------------------------------------------------------------
------------------------------------------------------------------------
-----      extra      --------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

decodeStr2LI : Json.Decode.Decoder a -> String -> Html b
decodeStr2LI decoder s =
    li [] [
      s ++ "  >>>  " ++ (Json.Decode.decodeString decoder s |> toString)
      |> text
    ]

decodeTree2Text2LI : String -> Html a
decodeTree2Text2LI s =
    decodeStr2LI decodeRecordTree s

decodeRecord2Text2LI : String -> Html a
decodeRecord2Text2LI s =
    decodeStr2LI decodeRecord s

recordsLI : Html a
recordsLI =
  li [] [ text "Records:", ul [] [
      decodeRecord2Text2LI """{"id":3, "tree":{"name": "joe"}}"""
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

---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------}
