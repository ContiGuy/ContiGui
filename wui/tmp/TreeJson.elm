module Util.TreeJson exposing (..)

import Util.TreeFlat exposing (..)

import Array
import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing (..)

import Html exposing (..)

encRec : Util.TreeFlat.Record -> Json.Encode.Value
encRec rec =
  Json.Encode.object [
    ( "n", Json.Encode.string rec.n )
  ]

decRec : Json.Decode.Decoder Record
decRec =
  Json.Decode.object2 Record
    ( "n" := Json.Decode.string )
    ( withDefault (Node []) ( "kids" := Json.Decode.null (Node []) ) )


encWrap : Util.TreeFlat.Wrap -> Json.Encode.Value
encWrap wrap =
  Json.Encode.object [
    ( "rec",     encRec wrap.rec )
  , ( "id",      Json.Encode.int wrap.id )
  , ( "par-id",  Json.Encode.int wrap.parent )
  , ( "par-n",   Json.Encode.string wrap.parName )
  ]

decWrap : Json.Decode.Decoder Wrap
decWrap =
  Json.Decode.object4 Wrap
    ( "rec" := decRec )
    ( "id" := Json.Decode.int )
    ( "par-id" := Json.Decode.int )
    ( "par-n" := Json.Decode.string )

encWrapArray : Array.Array Util.TreeFlat.Wrap -> Json.Encode.Value
encWrapArray wraps_l =
  Json.Encode.array (Array.map encWrap wraps_l)

decWrapArray : Json.Decode.Decoder (Array.Array Wrap)
decWrapArray =
  Json.Decode.array decWrap

wraps2json : Array.Array Wrap -> String
wraps2json wraps_a =
  Json.Encode.encode 2 <| encWrapArray wraps_a

json2wraps : String -> Result String (Array.Array Wrap)
json2wraps json_s =
  Json.Decode.decodeString decWrapArray json_s


testRecsAsHtml : List Record -> Html a
testRecsAsHtml recs_l =
  let
    jsonize tree =
      tree |> flatten |> wraps2json
    dejsonize tree_json =
      case tree_json |> json2wraps of
        Err errMsg -> notFoundRec errMsg
        Ok wraps_a ->
          wraps_a |> deflatten

    deJsonizedWraps tree =
      case tree |> flatten |> wraps2json |> json2wraps of
        Err _ -> Array.empty
        Ok wraps_a -> wraps_a
    rejsonize tree =
      ( tree, tree |> deJsonizedWraps |> deflatten )
  in
    ulOf jsonize identity dejsonize recs_l


main : Html a
main =
  testRecsAsHtml [t1, t2, t5]

