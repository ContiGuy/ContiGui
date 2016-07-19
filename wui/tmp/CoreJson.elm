module Util.CoreJson exposing (..)

import Util.CoreData exposing (..)

import Array
import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing (..)

import Html exposing (..)

encRec : Record -> Json.Encode.Value
encRec rec =
  Json.Encode.object [
    ( "n", Json.Encode.string rec.n )
  ]

decRec : Json.Decode.Decoder Record
decRec =
  Json.Decode.object1 Record
    ( "n" := Json.Decode.string )
    --( withDefault (Node []) ( "kids" := Json.Decode.null (Node []) ) )



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

