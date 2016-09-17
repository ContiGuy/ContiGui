-- Copyright © 2016 ContiGuy mrcs.contiguy@mailnull.com
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

module Test.System exposing (..)

----import Widget          as W  exposing (..)
import JobType           --  exposing (..)
import Job           --  exposing (..)
----import RSync                 exposing (..)
--import Util.Status
--import Util.Debug

import Html                  exposing (..)
import Html.App
--import Html.Events           exposing (..)
--import Html.Attributes       exposing (..)
--import Http                  exposing (..)
--import Task
--import String
--import Json.Decode     as JD exposing ((:=))
--import Json.Encode     as JE


main : Program Never
main =
  Html.App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { results : List TestSample  -- ( Maybe TestResult, TestModel )
--  { result : ( Maybe TestResult, TestModel )
--  id        : String
--  --, cfgName   : String
----    output    : String
----  , debug     : Bool
--
--  -- widgets
----  , jobTypes  : JobType.Model
----  , rootNode  : W.Node
----  , rsync     : RSync.Model
--  , jobType   : JobType.Model
  }

type alias TestResult = Result String String
type alias TestModel  = JobType.Model
type alias TestSample = ( Maybe TestResult, TestModel )

init : (Model, Cmd Msg)
init =
  let
--    ( root, nodes ) = aRoot "RSync" [
--      fst RSync.init
--    ] (fmtList "rsync {{}} # ..." " ")
    ( jobType, jtCmd ) = JobType.init
  in
    ( Model
      [ ( Nothing, jobType )
      ]

--      "" -- False
--      jobType
    , Cmd.map JobTypeMsg jtCmd )


newJob jobType =
    let
        (jobType', jtm) = JobType.update (JobType.JobMsg <| Job.New jobType.name) jobType
    in
        (Nothing, jobType')

-- UPDATE

type Msg =
    JobTypeMsg Int JobType.Msg
--    | ToggleDebug Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        resList =
            List.map () model.results
    in
        { model
        | results = resList
        } ! []

updateJobType : Msg -> TestSample -> ( TestSample, Cmd Msg )
updateJobType msg sample =
    case msg of
      JobTypeMsg jtIdx jtMsg ->
        let
          ( newJT, cmd ) =
            JobType.update jtMsg <| snd sample  --  model.result
--            JobType.update jtMsg model.jobType
          res =
--            if newJT == (fst JobType.init) then
            if JobType.equal newJT (fst JobType.init) then
                Ok "Empty JobType"
            else
                Err <| toString newJT
        in
----          ( { model | jobType = newJT }
--          ( { model
----            | jobType = newJT
--            | result = (Just res, newJT)
--            }
          ( (Just res, newJT)
          , Cmd.map JobTypeMsg cmd
          )

--      ToggleDebug dbg ->
--            ( { model | debug = dbg }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  let
--    x = 3
--    jt = JobType.view model.jobType
--
--    wTreeLI w =
--      {----------------------------------------------------------
--      if model.debug then
--        Html.App.map CallRSync (W.nodeAsHtmlLI w)
--      else
--      ----------------------------------------------------------}
--        div [] []
--
--    dbg =
--      div []
--      [
----      {----------------------------------------------------------
----        h4 [] [ text "debug" ]
----      , ul [] (
----        [
------          li [] [ text (W.cmdOf model.rootNode) ]
------        , li [] [
------            label [] [ text "extensive" ]
------          , input [ type' "checkbox", onCheck ToggleDebug ] []
------          ]
----        ] ++ [ wTreeLI model.jobType.job.node ] )
----      ----------------------------------------------------------}
--      ]

--    initialJobType =
--        fst JobType.init
--    dbgInit =
--        Util.Debug.init
--    dbg =
--        { dbgInit
--        | state = "{ id = \"\", name = \"RSync\", jobs = [] }"
--        }
--    expJobType =
--        { initialJobType
--        | id = ""
--        , action = ""
--        , status = fst <| Util.Status.update (Util.Status.Add <| Util.Status.Status "Load existing Jobs" <| Ok "done") Util.Status.init
--        , debug = dbg
----        | status = Util.Status.init
--        }

    resHtml =
--        table [] <| List.concat <| List.indexedMap (\ i -> res -> resHtmlEntry i res) model.results
        table []
            <| List.concat
--            <| List.indexedMap (\ i -> res -> resHtmlEntry i res) model.results
            <| List.indexedMap resHtmlEntry model.results
    resHtmlEntry i result =
      let
        idx = toString i
      in
--        case fst model.result of
        case fst result of
            Nothing ->
                [ tr [] [ td [] [ text <| idx ++ ": Pending ..." ] ] ]
            Just res ->
                case res of
                    Ok msg ->
                        [ tr [] [ td [] [ text <| idx ++ ": OK: " ++ msg ] ] ]
                    Err msg ->
--                        table []
                        [ tr []
                          [ td [] [ text <| idx ++ ": expected" ]
                          , td [] [ JobType.viewModel <| fst JobType.init ]
--                          , td [] [ JobType.viewModel expJobType ]
--                          , td [] [ text <| toString expJobType ]
                          ]
                        , tr []
                          [ td [] [ text <| idx ++ ": actual" ]
                          , td [] [ JobType.viewModel <| snd result ]
--                          , td [] [ JobType.viewModel <| snd model.result ]
--                          , td [] [ text <| toString <| snd model.result ]
                          ]
                        ]
--                        JobType.view <| snd model.result
  in
    div []
    [ h2 [] [ text "System Test" ]
    , Html.App.map JobTypeMsg resHtml
    ]

--    text <| toString <| model
--    div []
--    [ table [] [ tr [] [ td [] [ Html.App.map JobTypeMsg jt ] ] ]
----    , dbg
--    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

