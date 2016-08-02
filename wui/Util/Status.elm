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

module Util.Status exposing (..)

--import Widget.Data.Type exposing (..)

import Html exposing (..)
import Html.Attributes




-- MODEL

type alias Model =  -- dataType =
    { history : List (Status)  --  dataType)
    , showErr : Int
    , showOk  : Int
    }

type alias Status =  -- dataType =
    { action : String
    , status : Result String String
--    , data   : dataType
    }

init : Model   -- dataType
init =
    Model [] 2 0


-- UPDATE

type Msg   -- dataType
    = Add (Status)  --  dataType)

--update : (Msg dataType) -> (Model dataType) -> (Model dataType, Cmd dataType)
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add status ->
            { model
            | history = status :: model.history
            } ! []


-- VIEW

--view : Model dataType -> Html (Msg dataType)
view : Model -> Html (Msg)
view model =
    let
        start = { errsLeft = model.showErr
                , oksLeft  = model.showOk
                , tr_l = []
                }
        end = List.foldl foldHelper start model.history
    in
    table []
    (
--        List.map (\e -> tr[] [ td [] [e] ])
--            <| List.map viewEntry model.history

        List.map (\e -> tr[] [ td [] [e] ]) end.tr_l
--            <| List.foldl foldHelper {} model.history
    )

--foldHelper statusEntry {errsLeft, oksLeft, tr_l} =
--foldHelper : Status -> {a | errsLeft, oksLeft, tr_l} ->
foldHelper
    : Status
    -> { state | errsLeft : number, oksLeft : number', tr_l : List (Html Msg) }
    -> { state | errsLeft : number, oksLeft : number', tr_l : List (Html Msg) }
foldHelper statusEntry state =
    case statusEntry.status of
        Ok msg ->
            if state.oksLeft > 0 then
                { state   -- errsLeft
                | oksLeft = state.oksLeft - 1
                , tr_l = state.tr_l ++
                  [ label
                    [ Html.Attributes.style [("color", "green")]
                    ]
                    [ text <| statusEntry.action ++ ": " ++ msg
                    ]
                  ]
                }
            else
                state
----            [ text <| toString <| statusEntry
        Err msg ->
            if state.errsLeft > 0 then
                { state   -- errsLeft
                | errsLeft = state.errsLeft - 1
                , tr_l = state.tr_l ++
                  [ label
                    [ Html.Attributes.style [("color", "red")]
                    ]
                    [ text <| statusEntry.action ++ ": " ++ msg
                    ]
                  ]
                }
            else
                state

--            label
--            [ Html.Attributes.style [("color", "red")]
--            ]
----            [ text <| toString <| statusEntry
--            [ text msg
--            ]


--foldl : (a -> b -> b) -> b -> List a -> b

--viewEntry : Status -> (errsLeft, oksLeft) -> (Html Msg, errsLeft, oksLeft)

--viewEntry : Status dataType -> Html dataType
viewEntry : Status -> Html Msg
viewEntry statusEntry =
    case statusEntry.status of
        Ok msg ->
--            div [] []
            label
            [ Html.Attributes.style [("color", "green")]
            ]
--            [ text <| toString <| statusEntry
            [ text msg
            ]
        Err msg ->
            label
            [ Html.Attributes.style [("color", "red")]
            ]
--            [ text <| toString <| statusEntry
            [ text msg
            ]
