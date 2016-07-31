module Widget.Data.Json exposing (..)

import Widget.Data.Type exposing (..)
import Widget.Data.Flat exposing (..)

import Array
import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing ((|:))


{---------------------------------------}
encRec : Record -> Json.Encode.Value
encRec record =
    Json.Encode.object
        [ ("id",    encodeId           record.id)
        , ("label", Json.Encode.string record.label)
        , ("descr", Json.Encode.string record.descr)
        , ("value", encodeValue        record.value)
        , ("fmtr",  encodeFormatter    record.fmtr)
        ]

decRec : Json.Decode.Decoder Record
decRec =
    Json.Decode.succeed Record
        |: ("id"    := decodeId)
        |: ("label" := Json.Decode.string)
        |: ("descr" := Json.Decode.string)
        |: ("value" := decodeValue)
        |: ("fmtr"  := decodeFormatter)
      --|: ("kids"  := decodeKids)
        --|: (Json.Decode.Extra.withDefault NoKids ("kids" := decodeKids))
        -- !! -- ("kids" := treeNode))

decodeId : Json.Decode.Decoder String
decodeId =
    Json.Decode.string

encodeId : Id -> Json.Encode.Value
encodeId id =
    Json.Encode.string id


encodeValue : Value -> Json.Encode.Value
encodeValue v =
  case v of
    BoolValue b ->
      Json.Encode.object [ ("bool", Json.Encode.bool b ) ]
    StringValue s required ->
      Json.Encode.object
      [ ("string", Json.Encode.string s )
      , ("required", Json.Encode.bool required )
      ]
--    RootCmd ->
--      --Json.Encode.object [ ("root", Json.Encode.null ) ]
--      Json.Encode.object [ ("root", Json.Encode.string "RootCmd" ) ]
--      --JE.string "root"

--    Group vert ->
--      --JE.object [ ("group", JE.bool vert ) ]
--      if vert then
--      JE.object [ ("group", JE.string "vertical" ) ]
--      else
--        JE.object [ ("group", JE.string "horizontal" ) ]
    Group orient ->
      --Json.Encode.object [ ("group", Json.Encode.bool vert ) ]
      case orient of
        Vertical ->
          Json.Encode.object [ ("group", Json.Encode.string "vertical" ) ]
        Horizontal ->
          Json.Encode.object [ ("group", Json.Encode.string "horizontal" ) ]
        Disoriented ->
          Json.Encode.object [ ("group", Json.Encode.string "Disoriented" ) ]
    Switch currSelectedKid ->
      Json.Encode.object [ ("switch", Json.Encode.string currSelectedKid ) ]

decodeValue : Json.Decode.Decoder Value
decodeValue =
  Json.Decode.oneOf [
    Json.Decode.object1 BoolValue ( "bool" := Json.Decode.bool )
  , Json.Decode.object2 StringValue
    ( "string"   := Json.Decode.string )
    ( "required" := Json.Decode.bool )
  --, Json.Decode.object1 identity ( "root" := Json.Decode.null RootCmd )
--  , Json.Decode.object1 rootCmd ( "root" := Json.Decode.string )
  --, Json.Decode.string ( null RootCmd )
  --, Json.Decode.object1 Group ( "group" := bool )
  , Json.Decode.object1 Group ( "group" := decodeOrientation )
  , Json.Decode.object1 Switch ( "switch" := Json.Decode.string )
  ]


encodeFormatter : Formatter -> Json.Encode.Value
encodeFormatter item =
    case item of
        BoolFmtr cmdTrue cmdFalse ->
            Json.Encode.object [
              ( "on", Json.Encode.string cmdTrue )
            , ( "off", Json.Encode.string cmdFalse )
            ]
        StringFmtr cmdFmt ->
            Json.Encode.object [
              ( "fmt", Json.Encode.string cmdFmt )
            ]
        KidsListFmtr cmdFmt listSep ->
            Json.Encode.object [
              ( "list_fmt", Json.Encode.string cmdFmt )
            , ( "list_sep", Json.Encode.string listSep )
            ]
        {----------------------------------------------------
        KidsByIdFmtr cmdFmt listSep ->
            Json.Encode.object [
              ( "ids_fmt", Json.Encode.string cmdFmt )
            , ( "list_sep", Json.Encode.string listSep )
            ]
        ----------------------------------------------------}
        SelectedKidFmtr ->
            Json.Encode.object [
              ( "selected", Json.Encode.string "" )
            ]

decodeFormatter : Json.Decode.Decoder Formatter
decodeFormatter =
    Json.Decode.oneOf [
      Json.Decode.object2 BoolFmtr        ( "on" := Json.Decode.string )       ( "off" := Json.Decode.string )
    , Json.Decode.object1 StringFmtr      ( "fmt" := Json.Decode.string )
    , Json.Decode.object2 KidsListFmtr    ( "list_fmt" := Json.Decode.string ) ( "list_sep" := Json.Decode.string )
    --, Json.Decode.object2 KidsByIdFmtr    ( "ids_fmt" := Json.Decode.string )  ( "list_sep" := Json.Decode.string )
    , Json.Decode.object1 selectedKidFmtr ( "selected" := Json.Decode.string )
    ]

selectedKidFmtr : String -> Formatter
selectedKidFmtr _ =
  SelectedKidFmtr


--rootCmd : String -> Value
--rootCmd _ =
--  RootCmd

decodeOrientation : Json.Decode.Decoder Orientation
decodeOrientation =
    Json.Decode.map decodeStringToOrientation Json.Decode.string

decodeStringToOrientation : String -> Orientation
decodeStringToOrientation s =
  case s of
    "vertical"    -> Vertical
    "horizontal"  -> Horizontal
    _             -> Disoriented




encWrap : Wrap -> Json.Encode.Value
encWrap wrap =
  Json.Encode.object [
    ( "rec",        encRec          wrap.rec )
  , ( "id",         Json.Encode.int wrap.id )
  , ( "parent_id",  Json.Encode.int wrap.parent )
  ]

decWrap : Json.Decode.Decoder Wrap
decWrap =
  Json.Decode.object3 Wrap
    ( "rec"        := decRec )
    ( "id"         := Json.Decode.int )
    ( "parent_id"  := Json.Decode.int )

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

decodeNode : Json.Decode.Decoder Node
decodeNode =
  Json.Decode.map deflatten decWrapArray

encodeNode : Node -> Json.Encode.Value
encodeNode node =
  flatten node |> encWrapArray
