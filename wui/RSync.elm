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

module RSync exposing (Model, Msg, init, update, viewHead, viewBody)

import Widget as W
import Widget.Data.Type exposing (..)
import Widget.Gen

import RSyncConfig exposing (..)

import ComboBox
import Util.Debug
import JobType

import Html              exposing (..)
import Html.App
import Html.Events       exposing (..)
import Html.Attributes   exposing (..)
import Http              exposing (..)
import Task
--import String
import Dict
import Json.Decode as JD exposing ((:=))
import Json.Encode


main : Program Never
main =
  Html.App.program {
    init          = init,
    view          = view,
    update        = update,
    subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { combo        : ComboBox.Model
  , lastErr      : Maybe Http.Error
  , lastOk       : Maybe String

  , output       : String
  , debug        : Util.Debug.Model

  -- widgets
  , currJobName  : String
  , rsyncRoots   : Dict.Dict String Node
  }


--   Local:  rsync [OPTION...] SRC... [DEST]
--
--   Access via remote shell:
--     Pull: rsync [OPTION...] [USER@]HOST:SRC... [DEST]
--     Push: rsync [OPTION...] SRC... [USER@]HOST:DEST
--
--   Access via rsync daemon:
--     Pull: rsync [OPTION...] [USER@]HOST::SRC... [DEST]
--           rsync [OPTION...] rsync://[USER@]HOST[:PORT]/SRC... [DEST]
--     Push: rsync [OPTION...] SRC... [USER@]HOST::DEST
--           rsync [OPTION...] SRC... rsync://[USER@]HOST[:PORT]/DEST
--
--   Usages with just one SRC arg and no DEST arg will list the source files instead of copying.

init : (Model, Cmd Msg)
init =
  let
    model =
      Model ComboBox.init Nothing (Just "started") "" Util.Debug.init
         "" Dict.empty
  in
    model ! []

initialRootNode : Node
initialRootNode =
    aRoot "RSync" [
      RSyncConfig.init
    ] (fmtList "rsync {{}} # ..." " ")

getRootNode : Model -> Node
getRootNode model =
  case Dict.get model.currJobName model.rsyncRoots of
--  case List.head
--       <| List.filter (\ j -> j.name == model.currJobName) model.jobs
--  of
    Nothing   -> initialRootNode
    Just root -> root

{-----------------------------------
-----------------------------------}



-- UPDATE

type Msg =
    CallWidget W.Msg
  | ComboMsg ComboBox.Msg
  | DebugMsg Util.Debug.Msg
  | JobSelect String
  | JobSaveRequested String
  | SaveSucceed SaveJobResult
  | SaveFail Http.Error
  | JobsLoadRequested
  | LoadJobsFail Http.Error
  | LoadJobsSucceed JobType.JobTypes

--    | ToggleDebug Bool

replaceRootNode : Model -> Node -> JobType.Job -> JobType.Job
replaceRootNode model newRootNode job =
  if job.name == model.currJobName then
    { job
    | root = newRootNode
    }
  else
    job

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      CallWidget wMsg ->
        let
          ( newRootNode, cmd, saveNeeded ) =
            W.update wMsg <| getRootNode model
          newRoots =
            Dict.insert model.currJobName newRootNode model.rsyncRoots
        in
          { model
          | rsyncRoots = newRoots
          , output     = ( (Widget.Gen.cmdOf newRootNode) ++ "  " ++ ", save=" ++ (toString saveNeeded) )
          , lastOk     = Nothing
          } ! [ Cmd.map CallWidget cmd ]

      ComboMsg cbMsg ->
        let
          ( newCombo, nCbMsg ) = ComboBox.update cbMsg model.combo
        in
          { model
          | combo = newCombo
          , lastOk = Nothing
          } ! [ Cmd.map ComboMsg nCbMsg ]

      DebugMsg dbgMsg ->
        let
          ( newDebug, nDbgMsg ) = Util.Debug.update dbgMsg model.debug
        in
          ( { model | debug = newDebug }
          , Cmd.map DebugMsg nDbgMsg
          )

      JobSelect str ->
        let
          msgStr = Debug.log "RSync.JobSelect" str
          ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Select msgStr) model.combo
          
          {-----------------------------------------------------------------------
          -----------------------------------------------------------------------}
        in
          { model
          | combo  = nCombo
          , currJobName = msgStr
          , lastOk = Nothing
          } ! [ Cmd.map ComboMsg nCbMsg ]

      JobSaveRequested str ->
        let
          msgStr = Debug.log "RSync.JobSaveRequested" str

          saveCmdMsg =
            saveJob msgStr model
        in
          { model
          | output = "RSync.JobSaveRequested '" ++ str ++ "' ..."
          , lastOk = Just ( "saving job " ++ str ++ " ..." )
          }
          ! [ saveCmdMsg ]

      SaveSucceed saveResult ->
        let
          ( nCombo, nCbMsg ) =
            ComboBox.update (ComboBox.Success saveResult.jobName) model.combo
        in
          { model
          | combo = nCombo
          , output = toString saveResult
          , lastErr = Nothing
          , lastOk = Just ( "job " ++ saveResult.jobName ++ " saved" )
          } ! [ Cmd.map ComboMsg nCbMsg ]

      SaveFail err ->
          { model
          | output = toString err
          , lastErr = Just err
          , lastOk = Nothing
          } ! []
      
{--------------------------------------
--------------------------------------}
      JobsLoadRequested ->
        let
          loadJobsCmdMsg =
            loadJobs model
        in
          { model
          | output = "RSync.JobsLoadRequested ..."
          , lastOk = Just "loading jobs ..."
          }
          ! [ loadJobsCmdMsg ]   -- Cmd.batch [ saveCmdMsg, xCmdMsg ] ]

      LoadJobsSucceed loadedJobs ->
        let
          rsyncJobsOnly jobType =
            let
              jtName = Debug.log "LoadJobsSucceed, found job type" jobType.name
            in
              jtName == "RSync"
          optJobType =
            List.filter rsyncJobsOnly loadedJobs.jobTypes |> List.head
          rsyncJobs =
            case optJobType of
              Just rsyncJT ->
                rsyncJT.jobs
              Nothing ->
                []
          jobNames =
            List.map .name rsyncJobs
            {-----------------------------------
            -----------------------------------}
          
          newOpts =
            ComboBox.NewOptions <| "" :: jobNames
          ( nCombo, nCbMsg ) =
            ComboBox.update newOpts model.combo
        in
          { model
          | combo = nCombo
          , output = toString loadedJobs
          --, jobs = rsyncJobs
          , rsyncRoots = Dict.fromList <| List.map (\ j -> (j.name, j.root) ) rsyncJobs
          , lastErr = Nothing
          , lastOk = Just "jobs loaded"
          } ! [ Cmd.map ComboMsg nCbMsg ]

      LoadJobsFail err ->
          { model
          | output = toString err
          , lastErr = Just err
          , lastOk = Nothing
          } ! []

{--------------------------------------}
loadJobs : Model -> Cmd Msg
loadJobs model =
  let
    url = "/jobs/RSync"
    httpCall = Http.get decodeJobTypes url
  in
    Task.perform LoadJobsFail LoadJobsSucceed httpCall

decodeJobTypes : JD.Decoder JobType.JobTypes
decodeJobTypes =
  let
    x = Debug.log "decodeJobTypes" 1
  in
    JobType.decodeJobTypes
--------------------------------------}

initJob : String -> Model -> JobType.Job
initJob jobName model =
  JobType.Job "" "" jobName "RSync" "dunno what to do !"
    <| getRootNode model

{--------------------------------------
--------------------------------------}


saveJob : String -> Model -> Cmd Msg
saveJob jobName model =
  let
    url = "/jobs/RSync"
    body_s =
      initJob jobName model
        |> JobType.encodeJob
        |> Json.Encode.encode 2
    
    postCall = Http.post decodeJobSaved url (Http.string body_s)
  in
    Task.perform SaveFail SaveSucceed postCall
--------------------------------------}



type alias SaveJobResult =
  { jsonId   : String
  , yamlId   : String
  , typeName : String
  , jobName  : String
  , cmd      : String
  }

decodeJobSaved : JD.Decoder SaveJobResult
decodeJobSaved =
  JD.object5 SaveJobResult
    ("json_id"   := JD.string)
    ("yaml_id"   := JD.string)
    ("type_name" := JD.string)
    ("job_name"  := JD.string)
    ("cmd"       := JD.string)

{--------------------------------------
--------------------------------------}


-- VIEW

view : Model -> Html.Html Msg
view model =
  let
    v = ""
  in
    Html.div [] [
{------------------------------------------------------------}
      Html.h2 [] [ Html.text "RSync" ]
    , viewHead "XxX" model True
    , viewBody model
    ]

viewBody : Model -> Html.Html Msg
viewBody model =
  let
    (n, v) = W.viewRoot <| getRootNode model
  in
    Html.div [] [
      Html.App.map CallWidget v
    , Html.App.map DebugMsg ( Util.Debug.viewDbgStr model.output model.debug )
    ]

{------------------------------------------------------------
------------------------------------------------------------}


selectJob : String -> Msg
selectJob str =
  JobSelect str

requestJobSave : String -> Msg
requestJobSave str =
  JobSaveRequested str


viewHead : String -> Model -> Bool -> Html.Html Msg
viewHead labelText model allowToSave =
  let
    errHtml =
      case model.lastErr of
        Just err ->
          Html.b [
            Html.Attributes.style [ ("color", "red") ]
          ] [
            Html.text ( "   !! " ++ ( toString err ) ++ " !!" )
          ]
        
        Nothing ->
          case model.lastOk of
            Just okStr ->
              Html.div [ Html.Attributes.style [
                ("color", "green")
              ] ] [
                Html.text ( "Ok: " ++ okStr )
              ]

            Nothing ->
              Html.div [ Html.Attributes.style [
                ("color", "green")
              ] ] [
                Html.text "."
              ]

    saveJobButtonText selection =
      Html.div [] [
        Html.text ( "Save " )
      , Html.em [] [ Html.text ( selection ) ]
      ]
  in
    Html.table [] [
      Html.tr [] [
        td [] [ label [] [ text "Jobs" ] ]
      , td [] [ button [ onClick JobsLoadRequested ] [ text "Load" ] ]
      , td [] [ ComboBox.viewOption "--" selectJob model.combo ]
      , td [] [ ComboBox.viewButton saveJobButtonText requestJobSave model.combo ]
      , td [] [ label [] [ text "New job name" ] ]
      , td [] [ Html.App.map ComboMsg ( ComboBox.viewField model.combo ) ]
      , td [] [ Html.App.map ComboMsg ( ComboBox.viewDbg model.combo ) ]
      ]
    , tr [] [
        td [ colspan 7 ] [ errHtml ]
      ]
    ]

{------------------------------------------------------------
viewDbgStr : Maybe String -> Html.Html Msg
viewDbg optErrStr =
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
------------------------------------------------------------}


{------------------------------------------------------------
  let
    wTreeLI w =
      if model.debug then
        Html.App.map CallWidget (W.nodeAsHtmlLI w)
      else
        div [] []

    dbg =
      div [] [
        h4 [] [ text "debug" ]
      , ul [] ( [
          li [] [ text (W.cmdOf model.root) ]
        , li [] [
            label [] [ text "extensive" ]
          , input [ type' "checkbox", onCheck ToggleDebug ] []
          ]
        ] ++ [ wTreeLI model.root ] )
      ]
    ( rootName, rootView ) = W.viewRoot model.root
    jobName = model.cfgName
    jobNameEmpty = String.isEmpty ( String.trim jobName )
  in
    div [] [
{------------------------------------------------------------
      h2 [] [ text rootName ]
    ,--}
      table [] [ tr [] [
        td [] [ label [] [ text "Configuration" ] ]
      , td [] [ input [
                  type' "text"
                , value model.cfgName
                , onInput EditCfgName
                ] [] ]
      , td [] [ button [ onClick Save, disabled jobNameEmpty ] [ text "Save" ] ]
      ] ]
    , Html.App.map CallWidget rootView
    , h3 [] [ text "Output" ]
    , text model.output
--    , dbg
    ]
------------------------------------------------------------}



{------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
{------------------------------------------------------------
    wTreeLI w =
      if model.debug then
        Html.App.map CallWidget (W.nodeAsHtmlLI w)
      else
        div [] []

    dbg =
      div [] [
        h4 [] [ text "debug" ]
      , ul [] ( [
          li [] [ text (W.cmdOf model.root) ]
        , li [] [
            label [] [ text "extensive" ]
          , input [ type' "checkbox", onCheck ToggleDebug ] []
          ]
        ] ++ [ wTreeLI model.root ] )
      ]
------------------------------------------------------------}
    ( rootName, rootView ) = W.viewRoot model.root
    jobName = model.cfgName
    jobNameEmpty = String.isEmpty ( String.trim jobName )
  in
    div [] [
{------------------------------------------------------------
      h2 [] [ text rootName ]
    ,--}
      table [] [ tr [] [
        td [] [ label [] [ text "Configuration" ] ]
      , td [] [ input [
                  type' "text"
                , value model.cfgName
                , onInput EditCfgName
                ] [] ]
      , td [] [ button [ onClick Save, disabled jobNameEmpty ] [ text "Save" ] ]
      ] ]
    , Html.App.map CallWidget rootView
    , h3 [] [ text "Output" ]
    , text model.output
--    , dbg
    ]
------------------------------------------------------------}


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

