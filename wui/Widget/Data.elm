-- Copyright © 2016 ElmGone mrcs.elmgone@mailnull.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Widget.Data exposing (..)

--import Html              exposing (..)
--import Html.Events       exposing (..)
--import Html.Attributes   exposing (..)
--import Html.App
import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing ((|:))

--import Regex as RX

--import String            exposing (..)
--import List              exposing (..)
--import Dict


-- MODEL

type alias Node =
  { id       : Id
  , label    : String
  , descr    : String
  , value    : Value
  , kids     : Kids
  , fmtr     : Formatter
  }

type alias Id = String

type Kids
  = KidsList ( List Node )

type Value
  = BoolValue Bool
  | StringValue String
  | RootCmd
  -- | Group Bool
  | Group Orientation
  | Switch Id


type Orientation
  = Vertical
  | Horizontal
  | Disoriented        -- Diagonal   -- NO OP


type Formatter
  = BoolFmtr String String
  | StringFmtr String
  | KidsListFmtr String String
  | KidsByIdFmtr String String
  | SelectedKidFmtr




{-------------------------------------------------------------------}
decodeNode : Json.Decode.Decoder Node
decodeNode =
    Json.Decode.succeed Node
        |: ("id" := decodeId)
        |: ("label" := Json.Decode.string)
        |: ("descr" := Json.Decode.string)
        |: ("value" := decodeValue)
        |: ("kids" := decodeKids)
        |: ("fmtr" := decodeFormatter)

encodeNode : Node -> Json.Encode.Value
encodeNode record =
    Json.Encode.object
        [ ("id", encodeId record.id)
        , ("label", Json.Encode.string record.label)
        , ("descr", Json.Encode.string record.descr)
        , ("value", encodeValue record.value)
        , ("kids", encodeKids record.kids)
        , ("fmtr", encodeFormatter record.fmtr)
        ]


decodeId : Json.Decode.Decoder String
decodeId =
    Json.Decode.string

encodeId : Id -> Json.Encode.Value
encodeId id =
    Json.Encode.string id


decodeValue : Json.Decode.Decoder Value
decodeValue =
  Json.Decode.oneOf [
    Json.Decode.object1 BoolValue ( "bool" := Json.Decode.bool )
  , Json.Decode.object1 StringValue ( "string" := Json.Decode.string )
  --, Json.Decode.object1 identity ( "root" := Json.Decode.null RootCmd )
  , Json.Decode.object1 rootCmd ( "root" := Json.Decode.string )
  --, Json.Decode.string ( null RootCmd )
  --, Json.Decode.object1 Group ( "group" := bool )
  , Json.Decode.object1 Group ( "group" := decodeOrientation )
  , Json.Decode.object1 Switch ( "switch" := Json.Decode.string )
  ]

rootCmd : String -> Value
rootCmd _ =
  RootCmd

decodeOrientation : Json.Decode.Decoder Orientation
decodeOrientation =
    Json.Decode.map decodeStringToOrientation Json.Decode.string

decodeStringToOrientation : String -> Orientation
decodeStringToOrientation s =
  case s of
    "vertical"    -> Vertical
    "horizontal"  -> Horizontal
    _             -> Disoriented


encodeValue : Value -> Json.Encode.Value
encodeValue v =
  case v of
    BoolValue b ->
      Json.Encode.object [ ("bool", Json.Encode.bool b ) ]
    StringValue s ->
      Json.Encode.object [ ("string", Json.Encode.string s ) ]
    RootCmd ->
      --Json.Encode.object [ ("root", Json.Encode.null ) ]
      Json.Encode.object [ ("root", Json.Encode.string "RootCmd" ) ]
      --JE.string "root"
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


decodeKids : Json.Decode.Decoder Kids
decodeKids =
    Json.Decode.map KidsList ( Json.Decode.list decodeNode )

encodeKids : Kids -> Json.Encode.Value
encodeKids kidsItem =
    case kidsItem of
        KidsList kids_l ->
            Json.Encode.list ( List.map encodeNode kids_l )


--type Formatter
--  = BoolFmtr String String
--  | StringFmtr String
--  | KidsListFmtr String String
--  | KidsByIdFmtr String String
--  | SelectedKidFmtr
decodeFormatter : Json.Decode.Decoder Formatter
decodeFormatter =
    Json.Decode.oneOf [
      Json.Decode.object2 BoolFmtr        ( "on" := Json.Decode.string )       ( "off" := Json.Decode.string )
    , Json.Decode.object1 StringFmtr      ( "fmt" := Json.Decode.string )
    , Json.Decode.object2 KidsListFmtr    ( "list_fmt" := Json.Decode.string ) ( "list_sep" := Json.Decode.string )
    , Json.Decode.object2 KidsByIdFmtr    ( "ids_fmt" := Json.Decode.string )  ( "list_sep" := Json.Decode.string )
    , Json.Decode.object1 selectedKidFmtr ( "selected" := Json.Decode.string )
    ]

selectedKidFmtr : String -> Formatter
selectedKidFmtr _ =
  SelectedKidFmtr


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
        KidsByIdFmtr cmdFmt listSep ->
            Json.Encode.object [
              ( "ids_fmt", Json.Encode.string cmdFmt )
            , ( "list_sep", Json.Encode.string listSep )
            ]
        SelectedKidFmtr ->
            Json.Encode.object [
              ( "selected", Json.Encode.string "" )
            ]


{--------------------------------------------------------------------
decodeKids : Json.Decode.Decoder Kids
decodeKids =
    let
        decodeToType string =
            case string of
                "KidsList ( List Node )" -> Result.Ok KidsList ( List Node )
                _ -> Result.Err ("Not valid pattern for decoder to Kids. Pattern: " ++ (toString string))
    in
        Json.Decode.customDecoder Json.Decode.string decodeToType

encodeKids : Kids -> Json.Encode.Value
encodeKids item =
    case item of
        KidsList ( List Node ) -> Json.Encode.string "KidsList ( List Node )"


decodeValue : Json.Decode.Decoder Value
decodeValue =
    let
        decodeToType string =
            case string of
                "BoolValue Bool" -> Result.Ok BoolValue Bool
                "StringValue String" -> Result.Ok StringValue String
                "RootCmd" -> Result.Ok RootCmd
                "Group Bool" -> Result.Ok Group Bool
                "Switch Id" -> Result.Ok Switch Id
                _ -> Result.Err ("Not valid pattern for decoder to Value. Pattern: " ++ (toString string))
    in
        Json.Decode.customDecoder Json.Decode.string decodeToType

encodeValue : Value -> Json.Encode.Value
encodeValue item =
    case item of
        BoolValue Bool -> Json.Encode.string "BoolValue Bool"
        StringValue String -> Json.Encode.string "StringValue String"
        RootCmd -> Json.Encode.string "RootCmd"
        Group Bool -> Json.Encode.string "Group Bool"
        Switch Id -> Json.Encode.string "Switch Id"


decodeFormatter : Json.Decode.Decoder Formatter
decodeFormatter =
    let
        decodeToType string =
            case string of
                "BoolFmtr String String" -> Result.Ok BoolFmtr String String
                "StringFmtr String" -> Result.Ok StringFmtr String
                "KidsListFmtr String String" -> Result.Ok KidsListFmtr String String
                "KidsByIdFmtr String String" -> Result.Ok KidsByIdFmtr String String
                "SelectedKidFmtr" -> Result.Ok SelectedKidFmtr
                _ -> Result.Err ("Not valid pattern for decoder to Formatter. Pattern: " ++ (toString string))
    in
        Json.Decode.customDecoder Json.Decode.string decodeToType

encodeFormatter : Formatter -> Json.Encode.Value
encodeFormatter item =
    case item of
        BoolFmtr cmdTrue cmdFalse -> Json.Encode.string "BoolFmtr String String"
        StringFmtr cmdFmt -> Json.Encode.string "StringFmtr String"
        KidsListFmtr cmdFmt listSep -> Json.Encode.string "KidsListFmtr String String"
        KidsByIdFmtr cmdFmt listSep -> Json.Encode.string "KidsByIdFmtr String String"
        SelectedKidFmtr -> Json.Encode.string "SelectedKidFmtr"
-------------------------------------------------------------------}
