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

module JobType exposing (..)

import Html            exposing (..)
--import Html.Attributes
import Html.App
import Html.Events
import Http
import Task
import Cmd.Extra
import Json.Decode          exposing ((:=))
import Json.Decode.Extra    exposing ((|:))

--import HttpBuilder exposing (..)

import Job
import ComboBox
import Util.Debug
import Util.Status


-- MODEL

type alias Model =
  { id           : String
  , name         : String
  , job          : Job.Model
  , jobIdNames   : List (String, String)
  , combo        : ComboBox.Model
  , action       : String
  , status       : Util.Status.Model
  , debug        : Util.Debug.Model
  }

init : ( Model, Cmd Msg )
init =
    ( Model "rsync" "RSync" defaultJob
        [] emptyComboBox
        "Load existing Jobs"
        Util.Status.init
        Util.Debug.init
--    , loadJobs
    , putDefaultAndLoadJobs
    )

defaultJob : Job.Model
defaultJob =
    fst Job.init

emptyComboBox : ComboBox.Model
emptyComboBox =
    ComboBox.init []

equal : Model -> Model -> Bool
equal model1 model2 =
    model1.name == model2.name
    && model1.job == model2.job
    && model1.jobIdNames == model2.jobIdNames


-- UPDATE

type Msg
  = NewJob
  | CloneJob
  | SaveJob
  | UpgradeJob
  | Rename String
  | LoadJobs
  | LoadJobsFail Http.Error
  | LoadJobsSucceed Model
  | JobMsg Job.Msg
  | ComboMsg ComboBox.Msg
  | DebugMsg Util.Debug.Msg
  | StatusMsg Util.Status.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model', msg' ) =
          case msg of
            NewJob ->
                { model
                | action = "Create New Job"
                } !
                [ Cmd.map JobMsg <| Cmd.Extra.message <| Job.New model.name ]

            CloneJob ->
                { model
                | action = "Clone Job"
                } !
                [ Cmd.map JobMsg <| Cmd.Extra.message <| Job.Clone ]

            SaveJob ->
                { model
                | action = "Save Job"
                } !
                [ Cmd.map JobMsg <| Cmd.Extra.message <| Job.Save model.name "" ]

            UpgradeJob ->
                { model
                | action = "Upgrade Job"
                } !
                [ Cmd.map JobMsg <| Cmd.Extra.message <| Job.Upgrade ]

            Rename newName ->
              { model
              | name = newName
              } ! []

            DebugMsg dbgMsg ->
                let
                    ( newDebug, nDbgMsg ) = Util.Debug.update dbgMsg model.debug
                in
                    { model
                    | debug = newDebug
                    } ! [ Cmd.map DebugMsg nDbgMsg ]

            JobMsg jmsg ->
              let
                ( job', jmsg' ) =
                    Job.update jmsg model.job

                validJobIdNames =
                    newJobIdNames model job'

                newJobNames =
                    List.map (\ (id, n) -> n) validJobIdNames

                newOptionsMsgs =
                    case jmsg of
                        Job.SaveSucceed _ ->
                            [ Cmd.Extra.message <| ComboMsg <| ComboBox.NewOptions newJobNames
                            , Cmd.Extra.message <| StatusMsg <| Util.Status.Add <| Util.Status.Status model.action
                                                <| Ok <| "Job " ++ model.job.name ++ " saved"
                            ]
                        Job.SaveFail err ->
                            [ Cmd.Extra.message <| StatusMsg <| Util.Status.Add <| Util.Status.Status model.action
                                                <| Err <| "Failed to save Job " ++ model.job.name ++ " : "
                                                        ++ (toString err)
                            ]
                        _ ->
                            []
              in
                { model
                | job = job'
                , jobIdNames = validJobIdNames
                } !
                ( [ Cmd.map JobMsg jmsg' ] ++ newOptionsMsgs )

            ComboMsg cbmsg ->
              updateCombo cbmsg model

            LoadJobs ->
                { model
                | action = "Loading Jobs"
                } ! [ loadJobs ]

            LoadJobsFail err ->  -- Http.Error
                let
                    _ = Debug.log "JobType.LoadJobsFail" err
                in
                    model !
                    [ Cmd.Extra.message <| StatusMsg <| Util.Status.Add
                                        <| Util.Status.Status model.action
                                        <| Err <| toString err
                    ]

            LoadJobsSucceed jobType ->  -- (List Model)
                let
                    loadedJobNames =
                        List.map (\ (id, n) -> n) jobType.jobIdNames

                    (jobType', jobLoadMsgs) =
                        case List.head jobType.jobIdNames of
                            Nothing ->
                                ( jobType
                                , [ Cmd.Extra.message <| StatusMsg <| Util.Status.Add
                                                      <| Util.Status.Status model.action
                                                      <| Ok "done"
                                  ] )
                            Just (jobId, jobName ) ->
                                ( { jobType
                                  | action = "Load Job"
                                  }
                                , [ Cmd.Extra.message <| JobMsg <| Job.Load jobType.name jobId
                                  ] )
                in
                    jobType' !
                    ( [ Cmd.Extra.message <| ComboMsg <| ComboBox.NewOptions loadedJobNames
                      ] ++ jobLoadMsgs
                    )

            StatusMsg stMsg ->
                let
                    (status', stMsg') =
                        Util.Status.update stMsg model.status
                in
                    { model
                    | status = status'
                    } !
                    [ Cmd.map StatusMsg stMsg' ]


        ( debug', dbgMsg' ) =
            let
                modelStr = toString
                    { id = model'.id, name = model'.name, jobs = model'.jobIdNames }
            in
                Util.Debug.update (Util.Debug.Change modelStr) model'.debug
    in
        { model'
        | debug = debug'
        } !
        [ msg'
        , Cmd.map DebugMsg dbgMsg'
        ]
      {--------------------------------------------------------------
      --------------------------------------------------------------}

updateCombo : ComboBox.Msg -> Model -> (Model, Cmd Msg)
updateCombo cbmsg model =
      let
        oldJobName =
            "old job " ++ (toString model.job.id) ++ ": " ++ (toString model.job.name)
        ( cbb, cbmsg' ) =
            ComboBox.update cbmsg model.combo
        (nJob, msg2) =
          (
          case cbmsg of
            ComboBox.UpdateField s ->
                ( model.job
                , Cmd.map JobMsg <| Cmd.batch [ Cmd.Extra.message <| Job.Rename s ]
                )
            ComboBox.FieldChanged s ->
                model.job ! [ Cmd.Extra.message <| JobMsg <| Job.Save model.name "" ]
            ComboBox.Select s ->
              let
                newJobId = findJobId s model
              in
                ( model.job
                , Cmd.map JobMsg <| Cmd.batch [ Cmd.Extra.message <| Job.Save model.name newJobId ]
                )

            _ ->
                ( model.job
                , Debug.log ("combo other " ++ (toString cbmsg)) Cmd.none
                )
          )
      in
        { model
        | combo = cbb
        , job = nJob
        } !
        [ Cmd.map ComboMsg cbmsg'
        , msg2
        ]


findJobId : String -> Model -> String
findJobId jobName model =
    case List.filter (\(id, n) -> n == jobName) model.jobIdNames
        |> List.head
    of
        Nothing -> "---"
        Just (id, n) -> id



-- VIEW

view : Model -> Html Msg
view model =
      div []  --  Html.Events.onBlur <| JobMsg <| Job.Save model.name "" ]
      [ h2 [] [ text model.name ]
      , table []
        [ tr []
          [ td [] [ button [ Html.Events.onClick NewJob ]                              [ text "New"] ]
          , td [] [ button [ Html.Events.onClick CloneJob ]                            [ text "Clone"] ]
          , td [] [ button [ Html.Events.onClick SaveJob ]                             [ text "Save"] ]
          , td [] [ button [ Html.Events.onClick UpgradeJob ]                          [ text "Upgrade"] ]
          ]
        ]
      , Html.App.map StatusMsg <| Util.Status.view model.status
      , Html.App.map ComboMsg  <| ComboBox.view ["Job"] "--" ComboBox.Select model.combo
      , Html.App.map JobMsg    <| Job.view model.name model.job
      , Html.App.map DebugMsg  <| Util.Debug.view "JobType" model.debug
      ]

viewModel : Model -> Html Msg
viewModel model =
    table []
    [ tr []
      [ td [] [ text <| toString model.jobIdNames ]
      , td [] [ text <| toString model.job ]
      ]
    ]


-- Helpers

newJobIdNames : Model -> Job.Model -> List ( String, String )
newJobIdNames model job' =
  let
    otherId (id, n) =
        id /= model.job.id && id /= job'.id

    validId (id, n) =
        id /= ""

    otherJobIdNames =
        List.filter otherId model.jobIdNames

    newJobIdNames =
        if model.job.id == job'.id then
            (job'.id, job'.name) :: otherJobIdNames
        else
            (job'.id, job'.name)
                :: (model.job.id, model.job.name)
                :: otherJobIdNames

    validJobIdNames =
        List.filter validId newJobIdNames
  in
    validJobIdNames



putDefaultAndLoadJobs : Cmd Msg
putDefaultAndLoadJobs =
    let
        loadJobsCmd =
            loadJobs
--    url = "/jobs/RSync"
--    httpCall = Http.get decode url
    in
        Cmd.batch [
            Cmd.Extra.message (JobMsg Job.SaveDefault)
        ,   loadJobs
        ]
--    Task.perform LoadJobsFail LoadJobsSucceed httpCall

loadJobs : Cmd Msg
loadJobs =
  let
    url = "/jobs/RSync"
    httpCall = Http.get decode url
  in
    Task.perform LoadJobsFail LoadJobsSucceed httpCall

{----------------------------------------------}
--	JobTypes struct {
--		JobTypes []JobType `json:"job_types,omitempty"`
--	}
--
--	JobType struct {
--		//		Id   string `json:"id,omitempty"`
--		Id string `json:"id"`
--		//		Name string `json:"name,omitempty"`
--		Name string `json:"name"`
--		//		Jobs []Job `json:"jobs,omitempty"`
--		Jobs []Job `json:"jobs"` // `json:"jobs,omitempty"`
--	}
--
--	Job struct {
--		TypeName     string                 `json:"type_name,omitempty"`
--		Id           string                 `json:"job_id,omitempty"`
--		Name         string                 `json:"job_name,omitempty"`
--		JsonSha1     string                 `json:"json_id,omitempty"`
--		YamlSha1     string                 `json:"yaml_id,omitempty"`
--		Cmd          string                 `json:"cmd,omitempty"`
--		Debug        map[string]interface{} `json:"debug,omitempty"`
--		Nodes        []Wrap                 `json:"root"`
--		DefaultNodes []Wrap                 `json:"default_root,omitempty"`

--type alias Model =
--  { id           : String
--  , name         : String
--  , job          : Job.Model
--  , jobIdNames   : List (String, String)
--  , combo        : ComboBox.Model
--  , debug        : Util.Debug.Model
--  }

---- decode multiple/all job types
--decodeJobTypes : Json.Decode.Decoder Model
--decodeJobTypes =
--        ("job_types"        := decodeJobTypeList)
--
--decodeJobTypeList =
--    Json.Decode.map listHead
--        (Json.Decode.list decode)
--
--listHead list =
--    case List.head list of
--        Nothing -> Debug.log "decoding JobTypes FAILED - using empty default" (fst init)
--        Just jt -> Debug.log "decoded 1. JobType" jt

-- decode a single job type
decode : Json.Decode.Decoder Model
decode =
    Json.Decode.succeed Model
        |: ("id"            := Json.Decode.string)
        |: ("name"          := Json.Decode.string)
        |: ( Json.Decode.Extra.withDefault defaultJob
                ("job"           := Job.decode) )
        |: ("jobs"          := decodeJobIdNamesList)
        |: ( Json.Decode.Extra.withDefault emptyComboBox
                ("combo"         := Json.Decode.null emptyComboBox) )
        |: ( Json.Decode.Extra.withDefault ""
                ("action"         := Json.Decode.string) )
        |: ( Json.Decode.Extra.withDefault Util.Status.init
                ("status"         := Json.Decode.null Util.Status.init) )
        |: ( Json.Decode.Extra.withDefault Util.Debug.init
                ("debug"         := Json.Decode.null Util.Debug.init) )

decodeJobIdNamesList : Json.Decode.Decoder (List ( String, String ))
decodeJobIdNamesList =
    Json.Decode.map jobHeadList2IdNameTupleList (Json.Decode.list decodeJobHead)

jobHeadList2IdNameTupleList : List JobHead -> List (String, String)
jobHeadList2IdNameTupleList jobHeadList =
    List.map (\ jh -> (jh.id, jh.name) ) jobHeadList


-- decode only the head of a single job
type alias JobHead =
    { id       : String
    , name     : String
    , typeName : String
    }

decodeJobHead : Json.Decode.Decoder JobHead
decodeJobHead =
    Json.Decode.succeed JobHead
        |: ("job_id"        := Json.Decode.string)
        |: ("job_name"      := Json.Decode.string)
        |: ("type_name"     := Json.Decode.string)
----------------------------------------------}

