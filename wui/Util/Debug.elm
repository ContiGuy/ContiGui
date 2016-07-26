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

module Util.Debug exposing (..)

import Html -- exposing (..)
import Html.Events -- exposing (..)
import Html.Attributes -- exposing (..)
--import String

import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing ((|:))



-- MODEL

type alias Model =
    { debug      : Bool
    , state      : String
    }

{------------------------------
------------------------------}

init : Model
init =
  Model False ""


-- UPDATE

type Msg
  = ToggleDebug Bool
  | Change String


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleDebug dbg ->
      { model | debug = dbg } ! []
    Change s ->
      { model | state = s } ! []



-- VIEW

{------------------------------------------------------------------
------------------------------------------------------------------}

--viewDbgStr : String -> String -> Model -> Html.Html Msg
--viewDbgStr label errStr model =

view : String -> Model -> Html.Html Msg
view label model =
  let
    dbgInfoHtml =
      if model.debug then
          Html.text model.state
      else
          Html.div [] []
  in
      Html.div [] [
        Html.label []
        [ Html.text ("debug " ++ label)
        , Html.input
          [ Html.Attributes.type' "checkbox"
          , Html.Attributes.checked model.debug
          , Html.Events.onCheck ToggleDebug
          ] []
        , dbgInfoHtml
        ]
      ]

{------------------------------------------------------------------
viewDbgOptStr : Model -> Maybe String -> Html.Html Msg
viewDbgOptStr model optErrStr =
  let
    dbgInfoHtml =
      case optErrStr of
        Just errStr ->
          Html.text errStr
        Nothing ->
          Html.div [] []
  in
      Html.div [] [
        Html.label [] [ Html.text "debug" ]
      , Html.input [
          Html.Attributes.type' "checkbox"
          , Html.Attributes.checked model.debug
          , Html.Events.onCheck ToggleDebug
        ] []
      , dbgInfoHtml
      ]
------------------------------------------------------------------}

encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object (
        [ ("debug",    Json.Encode.bool model.debug)
        , ("state",    Json.Encode.string model.state)
        ]
      )


decode : Json.Decode.Decoder Model
decode =
    Json.Decode.succeed Model
        |: ("debug"    := Json.Decode.bool)
        |: ("state"    := Json.Decode.string)
