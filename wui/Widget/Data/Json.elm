module Widget.Data.Json exposing (..)

import Widget.Data.Type exposing (..)
import Widget.Data.Flat exposing (..)

import Array
import Json.Encode
import Json.Decode       exposing ((:=))
--import Json.Decode.Extra exposing (..)


{---------------------------------------}

encRec : Record -> Json.Encode.Value
encRec rec =
  Json.Encode.object [
    ( "n", Json.Encode.string rec.n )
  ]

decRec : Json.Decode.Decoder Record
decRec =
  Json.Decode.object1 Record
    ( "n" := Json.Decode.string )


encWrap : Wrap -> Json.Encode.Value
encWrap wrap =
  Json.Encode.object [
    ( "rec",     encRec wrap.rec )
  , ( "id",      Json.Encode.int wrap.id )
  , ( "par-id",  Json.Encode.int wrap.parent )
  ]

decWrap : Json.Decode.Decoder Wrap
decWrap =
  Json.Decode.object3 Wrap
    ( "rec" := decRec )
    ( "id" := Json.Decode.int )
    ( "par-id" := Json.Decode.int )
    --( "par-n" := Json.Decode.string )

encWrapArray : Array.Array Wrap -> Json.Encode.Value
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


{---------------------------------------
---------------------------------------}
