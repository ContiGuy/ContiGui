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

module Widget.Data_test exposing (..)

import Widget.Data exposing (..)
import Widget as W -- exposing (..)

import Html exposing (..)
--import Html.Events exposing (..)
--import Html.Attributes exposing (..)
import Html.App
--import Cmd.Extra
--import String exposing (..)
import Json.Encode
import Json.Decode


main : Program Never
main =
  Html.App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = \_ -> Sub.none
  }


-- MODEL

{----------------------------------------------
----------------------------------------------}
type alias Model =
    {
      fmtrs : List Widget.Data.Formatter
    , values : List Widget.Data.Value
    , nodes : List Node
    , kids : List Kids
    }




init : ( Model, Cmd Msg )
init =
  let
    root = W.aRoot "RSync" [
      --RSyncConfig.init
    ] (W.fmtList "rsync {{}} # ..." " ")
  in
    ( Model [
        BoolFmtr "T" "F"
--      , StringFmtr "sss fmt"
--      , KidsListFmtr "liste" ", "
--      , KidsByIdFmtr "idd" ", "
--      , SelectedKidFmtr
      ] [
        BoolValue True
--      , StringValue "sss val"
--      , RootCmd
--      , Group Horizontal
--      , Switch "X3"
      ] [ 
--          Node "aa_B" "label" "descr" (BoolValue False) (BoolFmtr "yes" "no") NoKids

--        W.aBool  "v" "Verbose"   "increase verbosity"                            "--verbose"
--      , W.aBooT  "r" "Recursive" "recurse into directories"                      "--recursive"
      ] [
        KidsList []
      , KidsList [ W.aBool "i" "a" "b" "c" ]
        -- Leaf, Kids []
      ]
    , Cmd.none )


-- UPDATE

type Msg
  = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  model ! []
  
{----------------------------------------------
  = ComboMsg ComboBox.Msg
  | Success String
  | Select String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ComboMsg cbMsg ->
      let
        ( nCombo, nCbMsg ) = ComboBox.update cbMsg model.combo
      in
        { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

    Success str ->
      let
        msgStr = Debug.log "CB.Test.Success" str
        ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Success msgStr) model.combo
      in
        { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

    Select str ->
      let
        msgStr = Debug.log "CB.Test.Select" str
        ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Select msgStr) model.combo
      in
        { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

{----------------------------------------
onSuccess : String -> ComboBox.Msg
onSuccess chosen =
  ComboBox.Success ( Debug.log "CBT success" chosen )
----------------------------------------}
----------------------------------------}

-- VIEW

view  : Model -> Html Msg
view model =
  div [] [
--    testTxt  -- ,text ( toString [ t1, t2, t3 ] )

    viewX "Formatter" Widget.Data.encodeFormatter Widget.Data.decodeFormatter model.fmtrs
  , viewX "Value" Widget.Data.encodeValue Widget.Data.decodeValue model.values
  , viewX "Kid" Widget.Data.encodeKids Widget.Data.decodeKids model.kids
--  , viewX "Tree" Widget.Data.encodeTree Widget.Data.decodeTree model.trees
  , viewX "Node" Widget.Data.encodeNode Widget.Data.decodeNode model.nodes
  ]

{----------------------------------------
----------------------------------------}
  

viewX : String -> (a -> Json.Encode.Value) -> Json.Decode.Decoder a -> List a -> Html Msg
viewX typeName encoder decoder values =
  let
    val2str f =
      f |> encoder
        |> Json.Encode.encode 0

    fmtr2LI f =
      li [] [ text ( ( toString f ) ++ " => " ++ ( val2str f ) ) ]

    str2fmtr2str s =
      s |> Json.Decode.decodeString decoder
        |> toString

    str2fmtr2LI f =
      let
        s = val2str f
      in
        li [] [ text ( s ++ " => " ++ ( str2fmtr2str s ) ) ]
  in
    div [] [
      h3 [] [ text ("encode " ++ typeName ++ "'s to JSON") ]
    , ul [] ( List.map fmtr2LI values )
--    , h3 [] [ text ("decode " ++ typeName ++ "'s from JSON") ]
--    , ul [] ( List.map str2fmtr2LI values )
    ]
  


{----------------------------------------
  table [] [ tr [] [
    td [] [ ComboBox.viewOption "--" select model.combo ]
  , td [] [ ComboBox.viewButton (\ s -> text ( "Save '" ++ s ++ "' !" ) ) success model.combo ]
  , td [] [ label [] [ text "Test: Pick new" ] ]
  , td [] [ Html.App.map ComboMsg ( ComboBox.viewField model.combo ) ]
  , td [] [ Html.App.map ComboMsg ( ComboBox.viewDbg model.combo ) ]
  ] ]

select : String -> Msg
select str =
  Select str

success : String -> Msg
success str =
  Success str
----------------------------------------------}
