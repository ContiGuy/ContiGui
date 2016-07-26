module Job exposing (..)

import Html              exposing (..)
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
  { id           : String
  , name         : String
  , typeName     : String
  , node         : Node
  , debug        : Util.Debug.Model
  }

init : ( Model, Cmd msg )
init =
    let
--        node = aBool "11" "22" "33" "44"
--        node = RSyncConfig.init
        node = aVertical "empty-job" "Empty Job" [] <| fmtList "<<EMPTY JOB -- DON'T USE>>" ", "
    in
        ( Model node.rec.id node.rec.label "Non Job Type" node Util.Debug.init --""
        , Cmd.none )

defaultRootNode : Node
defaultRootNode =
--    RSyncConfig.init
    RSyncConfig.fake



-- UPDATE

type Msg
  = Rename String
  | Save String String
  | New String
  | SaveSucceed Model
  | SaveFail Http.Error
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
                    sljCmd = newJob jobTypeName model   -- newJobId
--                    _ = Debug.log "Job.update:newJob" sljCmd
                in
                    model !
                    [ sljCmd
--                    , Cmd.Extra.message <| DebugMsg <| Util.Debug.Change <|"Job.New [" ++ jobTypeName ++ "]"
                    ]

            Rename newName ->
              { model
              | name = newName
              } ! []

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
                    _ = Debug.log "Job.update:saveLoadJob" sljCmd
                in
--                    ( model, sljCmd )
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
                    { id = model'.id, name = model'.name, jtype = model'.typeName }
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
  div []
  [ table []
    [
--      tr []
--      [ th [] [ text jobTypeName ]
--      , td [] [ text model.name ]
--      ]
--    ,
      tr []
      [ td [] [ Html.App.map WidgetMsg (Widget.view model.node) ]
      ]
--    , Html.App.map DebugMsg ( Util.Debug.viewDbgStr "Job" model.output model.debug )
    , Html.App.map DebugMsg ( Util.Debug.view "Job" model.debug )
    ]
  ]



-- Helpers

--saveLoadJob : String -> Model -> Cmd Msg
--saveLoadJob jobTypeName model =
--    saveLoadJobCall jobTypeName model
--        |> Task.perform SaveFail SaveSucceed

newJob : String -> Model -> Cmd Msg
newJob jobTypeName model =
  let
    _ = Debug.log "Job.newJob" model
  in
    newJobCall jobTypeName model -- newJobId
        |> Task.perform SaveFail SaveSucceed


newJobCall : String -> Model -> Task.Task Http.Error Model
newJobCall jobTypeName model =
  let
    url = "/jobs/RSync"
--      ++ ( if newJobId == "" then ""
--        else "?newJobId=" ++ newJobId
--      )
--        ++ "/" ++ job.id
    body_s =
      encodeJobX jobTypeName model
--        [ ("default_root",      Widget.Data.Json.encodeNode RSyncConfig.init)
----        [ ("default_root",      Widget.Data.Json.encodeNode RSyncConfig.fake)
        [ ("default_root",      Widget.Data.Json.encodeNode defaultRootNode)
        ]
        |> Json.Encode.encode 2
  in
    Http.post decodeJob url (Http.string body_s)

--saveLoadJob : String -> Model -> Cmd Msg
--saveLoadJob jobTypeName model =
--    saveLoadJobCall jobTypeName model
--        |> Task.perform SaveFail SaveSucceed

saveLoadJob : String -> Model -> String -> Cmd Msg
saveLoadJob jobTypeName model newJobId =
  let
    _ = Debug.log "Job.saveLoadJob" model
  in
    saveLoadJobCall jobTypeName model newJobId
        |> Task.perform SaveFail SaveSucceed


--saveLoadJobCall : String -> Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response (Model))
saveLoadJobCall : String -> Model -> String -> Task.Task Http.Error Model
--saveLoadJobCall jobTypeName job =
saveLoadJobCall jobTypeName model newJobId =
  let
    url = "/jobs/RSync" ++ "/" ++ model.id ++
      ( if newJobId == "" then ""
        else "?newJobId=" ++ newJobId
      )
    body_s =
--      initJob jobName model
--      job
--        |>
      encodeJob jobTypeName model
        |> Json.Encode.encode 2
  in
    httpSend "PUT" decodeJob url (Http.string body_s)

httpSend
    : String
    -> Json.Decode.Decoder a
    -> String
    -> Http.Body
    -> Task.Task Http.Error a
httpSend verb decoder url body =
  let request =
        { verb = verb   -- "POST"
        , headers = []
        , url = url
        , body = body
        }
  in
      Http.fromJson decoder (Http.send Http.defaultSettings request)


----saveLoadJobCall : Json.Decode.Decoder node -> String -> Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response (Model))
----saveLoadJobCall decodeNode jobTypeName job =
--saveLoadJobCall : String -> Model -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response (Model))
--saveLoadJobCall jobTypeName job =
--  HttpBuilder.post "/jobs/RSync"
--    |> withJsonBody (encodeJob jobTypeName job)
--    |> withHeader "Content-Type" "application/json"
--    |> withTimeout (10 * Time.second)
--    |> withCredentials
--    |> send (jsonReader (decodeJob)) stringReader


{----------------------------------------------}
--decodeJob : Json.Decode.Decoder node -> Json.Decode.Decoder Model
--decodeJob decodeNode =
decodeJob : Json.Decode.Decoder Model
decodeJob =
--  { id           : String
--  , name         : String
--  , node         : Node
--  , debug        : Util.Debug.Model
--  , output       : String
--  }
    Json.Decode.succeed Model
--        |: ("json_id"   := Json.Decode.string)
--        |: ("yaml_id"   := Json.Decode.string)
        |: ("job_id"        := Json.Decode.string)
        |: ( Json.Decode.Extra.withDefault "" ("job_name"  := Json.Decode.string) )
        |: ("type_name" := Json.Decode.string)
--        |: ("cmd"       := Json.Decode.string)
--        |: ("root"      := decodeNode)
        |: ("root"      := Widget.Data.Json.decodeNode)
--        |: ("debug"     := Json.Decode.bool)
--        |: ("output"    := Json.Decode.string)
        |: ( Json.Decode.Extra.withDefault Util.Debug.init ("debug"  := Util.Debug.decode)  )
--        |: ( Json.Decode.Extra.withDefault "" ("output"    := Json.Decode.string))
----------------------------------------------}




--encodeJob : (node -> Json.Encode.Value) -> String -> Model -> Json.Encode.Value
--encodeJob encodeNode jobTypeName job =
encodeJob : String -> Model -> Json.Encode.Value
encodeJob jobTypeName model =
    encodeJobX jobTypeName model [
        ("root",      Widget.Data.Json.encodeNode model.node)
    ]

--    Json.Encode.object
----        [ ("json_id",   Json.Encode.string job.jsonId)
----        , ("yaml_id",   Json.Encode.string job.yamlId)
--        [ ("id",        Json.Encode.string job.id)
--        , ("job_name",  Json.Encode.string job.name)
--        , ("type_name", Json.Encode.string jobTypeName)
--        , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf job.node)
----        , ("root",      encodeNode job.node)
--        , ("root",      Widget.Data.Json.encodeNode job.node)
--        ]

--encodeJobX : String -> Model -> Json.Encode.Value
encodeJobX
    : String
    -> Model  --  { a | id : String, name : String, node : Node }
    -> List ( String, Json.Encode.Value )
    -> Json.Encode.Value
encodeJobX jobTypeName model addFields =
    Json.Encode.object (
--        [ ("json_id",   Json.Encode.string job.jsonId)
--        , ("yaml_id",   Json.Encode.string job.yamlId)
        [ ("job_id",    Json.Encode.string model.id)
        , ("job_name",  Json.Encode.string model.name)
        , ("type_name", Json.Encode.string jobTypeName)
        , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf model.node)
--        , ("root",      encodeNode job.node)
--        , ("root",      Widget.Data.Json.encodeNode job.node)
        , ("debug",     Util.Debug.encode model.debug)
        ] ++ addFields
      )
