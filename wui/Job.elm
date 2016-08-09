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

module Job exposing (..)

import Html              exposing (..)
--import Html.Attributes
import Html.App
import Http
import Task
--import Time
import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Cmd.Extra

--import HttpBuilder       exposing (..)

import Widget.Data.Type  exposing (..)
import Widget.Data.Json
import Widget.Gen
import Widget
import Util.Debug
import RSyncConfig       exposing (..)



-- MODEL

type alias Model =
  { id              : String
  , name            : String
  , typeName        : String
  , scriptTemplate  : String
  , node            : Node
  , debug           : Util.Debug.Model
  , scriptFile      : String
  , lostFoundRecs   : List Record
  }

init : ( Model, Cmd msg )
init =
    let
--        node = RSyncConfig.init
        node = aVertical "new-job" "new" [] <| fmtList "<<EMPTY JOB -- DON'T USE>>" ", "
    in
--        ( Model node.rec.id node.rec.label "Empty Job Type" node Util.Debug.init
        ( Model "" node.rec.label "Empty Job Type" defaultScript node Util.Debug.init "empty-file" []
        , Cmd.none )

defaultRootNode : Node
defaultRootNode =
    RSyncConfig.init
--    RSyncConfig.fake

defaultRootEncTuple : ( String, Json.Encode.Value )
defaultRootEncTuple =
--    ("default_root", Widget.Data.Json.encodeNode defaultRootNode)
--    encTuple "default_root" defaultRootNode
    encDefaultRootTuple defaultRootNode

encDefaultRootTuple : Node -> ( String, Json.Encode.Value )
encDefaultRootTuple node =
--    ("default_root", Widget.Data.Json.encodeNode defaultRootNode)
    encTuple "default_root" node

encTuple : String -> Node -> ( String, Json.Encode.Value )
encTuple name node =
    (name, Widget.Data.Json.encodeNode node)

defaultScript : String
defaultScript =
    RSyncConfig.scriptTemplate


-- UPDATE

type Msg
  = Rename String
  | New String
  | Upgrade
  | Clone
  | Save String String
  | SaveSucceed Model
  | SaveFail Http.Error
  | Load String String
  | LoadFail Http.Error
  | LoadSucceed Model
  | WidgetMsg Widget.Msg
  | DebugMsg Util.Debug.Msg

--update : Msg -> { mdl | name : String } -> ( { mdl | name : String }, Cmd msg )
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model', cmdMsg ) =
          case msg of
            New jobTypeName ->
                let
                    njCmd =
                        newJob jobTypeName model defaultRootEncTuple ""
--                    _ = Debug.log "Job.update:newJob" sljCmd
                in
                    model !
                    [ njCmd
--                    , Cmd.Extra.message <| DebugMsg <| Util.Debug.Change <|"Job.New [" ++ jobTypeName ++ "]"
                    ]

            Clone ->
                let
                    cjCmd =
                        newJob model.typeName model
                            (encDefaultRootTuple model.node) model.name
--                    _ = Debug.log "Job.update:newJob" sljCmd
                in
                    model ! [ cjCmd ]

            Rename newName ->
              { model
              | name = newName
              } ! []

            Upgrade ->
                let
                    (node', lfRecs) =
                        Widget.Gen.upgrade defaultRootNode model.node
                    _ = Debug.log "lost+found" lfRecs
                in
                  { model
                  | node = node'
                  , scriptTemplate = defaultScript
                  , lostFoundRecs = lfRecs ++ model.lostFoundRecs
                  } !
                  [ Cmd.Extra.message <| Save "" "" ]

            WidgetMsg wm ->
                let
                    (node', wm') =
                        Widget.update wm model.node
                in
                    { model
                    | node = node'
                    } !
                    [ Cmd.map WidgetMsg wm'
                    , Cmd.Extra.message <| Save "" ""
                    ]

            Save jobTypeName newJobId ->
                let
                    jobType =
                        if jobTypeName == "" then model.typeName
                        else jobTypeName
                    sljCmd = saveLoadJob jobType model newJobId
--                    _ = Debug.log "Job.update:saveLoadJob" sljCmd
                in
                    { model
                    | typeName = jobType
                    } ! [ sljCmd ]

            SaveSucceed newModel ->   -- Model ->
                newModel ! []
        --        let
        --          ( nCombo, nCbMsg ) =
        --            ComboBox.update (ComboBox.Success saveResult.jobName) model.combo
        --        in
        --          { model
        --          | combo = nCombo
        --          , output = toString saveResult
        --          , lastErr = Nothing
        --          , lastOk = Just ( "job " ++ saveResult.jobName ++ " saved" )
        --          } ! [ Cmd.map ComboMsg nCbMsg ]

            SaveFail err ->   -- Http.Error ->
                model ! [
                  Cmd.Extra.message <| DebugMsg <| Util.Debug.Change <| "Job.SaveFail: " ++ (toString err)
                ]
        --          { model
        --          | output = toString err
        --          , lastErr = Just err
        --          , lastOk = Nothing
        --          } ! []

            Load jobTypeName newJobId ->
                let
                    jobType =
                        if jobTypeName == "" then model.typeName
                        else jobTypeName
                    ljCmd = loadJob jobType newJobId
--                    _ = Debug.log "Job.update:loadJob" ljCmd
                in
                    { model
                    | typeName = jobType
                    } ! [ ljCmd ]

            LoadSucceed newModel ->   -- Model ->
              let
                _ = Debug.log "Job.LoadSucceed" (newModel.name, newModel.id)
              in
                newModel ! []
        --        let
        --          ( nCombo, nCbMsg ) =
        --            ComboBox.update (ComboBox.Success saveResult.jobName) model.combo
        --        in
        --          { model
        --          | combo = nCombo
        --          , output = toString saveResult
        --          , lastErr = Nothing
        --          , lastOk = Just ( "job " ++ saveResult.jobName ++ " saved" )
        --          } ! [ Cmd.map ComboMsg nCbMsg ]

            LoadFail err ->   -- Http.Error ->
              let
                _ = Debug.log "Job.LoadFail" err
              in
                model ! [
                  Cmd.Extra.message <| DebugMsg <| Util.Debug.Change <| "Job.LoadFail: " ++ (toString err)
                ]
        --          { model
        --          | output = toString err
        --          , lastErr = Just err
        --          , lastOk = Nothing
        --          } ! []

            DebugMsg dbgMsg ->
                let
                    ( newDebug, nDbgMsg ) = Util.Debug.update dbgMsg model.debug
                in
                    { model
                    | debug = newDebug
                    } ! [ Cmd.map DebugMsg nDbgMsg ]

        ( debug', dbgMsg' ) =
            let
                modelStr = toString
                    { id = model'.id, name = model'.name, jtype = model'.typeName }  -- , node = model'.node }
            in
                Util.Debug.update (Util.Debug.Change modelStr) model'.debug
--        _ = Debug.log "Job.update.debug" debug'
    in
        { model'
        | debug = debug'
        } !
        [ cmdMsg
        , Cmd.map DebugMsg dbgMsg'
        ]


-- VIEW

view : String -> Model -> Html Msg
view jobTypeName model =
    let
        lostFound =
            if List.length model.lostFoundRecs == 0 then
                []
            else
                [ h4 [] [ text "Lost + Found" ]
                ] ++
                  List.map (\lfr ->
                    Html.App.map WidgetMsg <| Widget.viewRecord (text <| toString <| lfr) lfr
                  ) model.lostFoundRecs
--                , tr [] [ td [] [ table []
--                      [ tr [] [ td [] [ text "command:" ],     td [] [ em [] [ text <| Widget.Gen.cmdOf model.node ] ] ]
--                      , tr [] [ td [] [ text "script file:" ], td [] [ em [] [ text model.scriptFile ] ] ]
--                      ] ] ]
--                ]
    in
      div []
      [ table [] (
        [ tr [] [ td [] [ Html.App.map WidgetMsg (Widget.view model.node) ] ]
        ] ++ lostFound ++
        [ h4 [] [ text "Details" ]
        , tr [] [ td [] [ table []
              [ tr [] [ td [] [ text "command:" ],     td [] [ em [] [ text <| Widget.Gen.cmdOf model.node ] ] ]
              , tr [] [ td [] [ text "script file:" ], td [] [ em [] [ text model.scriptFile ] ] ]
              ] ] ]
--        ] ++   -- lostFound ++
--        [ Html.App.map DebugMsg ( Util.Debug.view "Job" model.debug )
        , Html.App.map DebugMsg ( Util.Debug.view "Job" model.debug )
        ] )
      ]



-- Helpers

newJob : String -> Model -> ( String, Json.Encode.Value ) -> String -> Cmd Msg
newJob jobTypeName model defRootEncTuple newJobName =
--  let
--    _ = Debug.log "Job.newJob" model
--  in
    newJobCall jobTypeName model defRootEncTuple newJobName
        |> Task.perform SaveFail SaveSucceed


--saveLoadJobCall : String -> Model ->  String -> Task.Task Http.Error Model
--loadJobCall :     String ->           String -> Task.Task Http.Error Model
--newJobCall :      String -> Model ->            Task.Task Http.Error Model

newJobCall : String -> Model -> ( String, Json.Encode.Value ) -> String -> Task.Task Http.Error Model
newJobCall jobTypeName model defRootEncTuple newJobName =
  let
    _ = Debug.log "Job.newJobCall" (jobTypeName, newJobName, model, defRootEncTuple)

    url = "/jobs/RSync" ++
        ( if newJobName == "" then ""
          else "?newJobName=" ++ newJobName
        )
--    (nodes, encode) =
--        if model.id == "" then
--            ( []
--            , encodeNewJobOfType jobTypeName
--            )
--        else
--            ( [ ("root", Widget.Data.Json.encodeNode model.node) ]
--            , encodeX jobTypeName model
--            )

--    defaultRootEncTuple =
--        ("default_root", Widget.Data.Json.encodeNode defaultRootNode)
    (toJsonString, extraNodes) =
        if model.id == "" then
            ( encodeNewJobOfType jobTypeName model.scriptTemplate, [] )
        else
            ( encodeX jobTypeName model
            , [ ("root", Widget.Data.Json.encodeNode model.node) ]
            )
    body_s =
--        toJsonString ( ("default_root", Widget.Data.Json.encodeNode defaultRootNode) :: nodes )
        toJsonString
--            ( ("default_root", Widget.Data.Json.encodeNode defaultRootNode) :: extraNodes )
            ( defRootEncTuple :: extraNodes )
            |> Json.Encode.encode 2
--        if model.id == "" then
--            ( encodeNewJobOfType jobTypeName
--                [ ("default_root",      Widget.Data.Json.encodeNode defaultRootNode)
--                ]
--                |> Json.Encode.encode 2
--            )
--        else
--            ( encodeX jobTypeName model
--                [ ("root",          Widget.Data.Json.encodeNode model.node)
--                , ("default_root",  Widget.Data.Json.encodeNode defaultRootNode)
--                ]
--                |> Json.Encode.encode 2
--            )
  in
    Http.post decode url (Http.string body_s)

loadJob : String -> String -> Cmd Msg
loadJob jobTypeName newJobId =
--  let
--    _ = Debug.log "Job.loadJob" model
--  in
    loadJobCall jobTypeName newJobId
        |> Task.perform LoadFail LoadSucceed


loadJobCall : String -> String -> Task.Task Http.Error Model
loadJobCall jobTypeName newJobId =
  let
    url = "/jobs/RSync" ++ "/" ++ newJobId
  in
    Http.get decode url

saveLoadJob : String -> Model -> String -> Cmd Msg
saveLoadJob jobTypeName model newJobId =
--  let
--    _ = Debug.log "Job.saveLoadJob" model
--  in
    saveLoadJobCall jobTypeName model newJobId
        |> Task.perform SaveFail SaveSucceed


saveLoadJobCall : String -> Model -> String -> Task.Task Http.Error Model
saveLoadJobCall jobTypeName model newJobId =
  let
    url = "/jobs/RSync" ++ "/" ++ model.id ++
      ( if newJobId == "" then ""
        else "?newJobId=" ++ newJobId
      )
    body_s =
      encode jobTypeName model
        |> Json.Encode.encode 2
  in
    httpSend "PUT" decode url (Http.string body_s)

httpSend
    : String
    -> Json.Decode.Decoder a
    -> String
    -> Http.Body
    -> Task.Task Http.Error a
httpSend verb decoder url body =
  let request =
        { verb = verb
        , headers = []
        , url = url
        , body = body
        }
  in
      Http.fromJson decoder (Http.send Http.defaultSettings request)


{----------------------------------------------}
decode : Json.Decode.Decoder Model
decode =
    Json.Decode.succeed Model
        |: ("job_id"        := Json.Decode.string)
        |: ( Json.Decode.Extra.withDefault "" ("job_name"  := Json.Decode.string) )
        |: ("type_name" := Json.Decode.string)
--        |: ("cmd"       := Json.Decode.string)
        |: ( Json.Decode.Extra.withDefault defaultScript ("script_template" := Json.Decode.string)  )
--        |: ("script"    := Json.Decode.string)
--        |: ("root"      := decodeNode)
        |: ("root"      := Widget.Data.Json.decodeNode)
--        |: ("debug"     := Json.Decode.bool)
--        |: ("output"    := Json.Decode.string)
        |: ( Json.Decode.Extra.withDefault Util.Debug.init ("debug"  := Util.Debug.decode)  )
--        |: ( Json.Decode.Extra.withDefault "" ("output"    := Json.Decode.string))
        |: ( Json.Decode.Extra.withDefault "" ("script_fpath"  := Json.Decode.string)  )
        |: ( Json.Decode.Extra.withDefault [] ("lost+found" := (Json.Decode.list Widget.Data.Json.decRec) ) )
----------------------------------------------}




--encodeJob : (node -> Json.Encode.Value) -> String -> Model -> Json.Encode.Value
--encodeJob encodeNode jobTypeName job =
encode : String -> Model -> Json.Encode.Value
encode jobTypeName model =
    encodeX jobTypeName model [
        ("root",      Widget.Data.Json.encodeNode model.node)
    ]


encodeX
    : String
    -> Model
    -> List (String, Json.Encode.Value)
    -> Json.Encode.Value
encodeX jobTypeName model addFields =
    encodeNewJobOfType jobTypeName model.scriptTemplate
    ( [ ("job_id",    Json.Encode.string model.id)
      , ("job_name",  Json.Encode.string model.name)
--      , ("script_template",    Json.Encode.string model.scriptTemplate)
      , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf model.node)
      , ("debug",     Util.Debug.encode model.debug)
      ] ++ addFields
    )

encodeNewJobOfType
    : String -> String
    -> List ( String, Json.Encode.Value )
    -> Json.Encode.Value
encodeNewJobOfType jobTypeName scriptTemplate jobFields =
    Json.Encode.object (
        [ ("type_name", Json.Encode.string jobTypeName)
        , ("script_template",    Json.Encode.string scriptTemplate)
        ] ++ jobFields
      )
