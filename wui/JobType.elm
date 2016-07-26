module JobType exposing (..)

import Html     exposing (..)
import Html.App
import Html.Events
--import Dict     exposing (..)

--import Http
--import Task
--import Time
--import Json.Encode
--import Json.Decode
import Cmd.Extra

--import HttpBuilder exposing (..)

import Job
import ComboBox
--import Widget.Data.Json
import Util.Debug


-- MODEL

type alias Model =
  { id           : String
  , name         : String
  , job          : Job.Model
--  , jobIdsByName : Dict.Dict String String
--  , jobNamesById : Dict.Dict String String
  , jobIdNames   : List (String, String)
  , combo        : ComboBox.Model
  , debug        : Util.Debug.Model
  }

init : ( Model, Cmd msg )
init =
  let
--    ( cb, cm ) =
--      ComboBox.initWith (Dict.values jobNamesById)
--      ComboBox.initWith (Dict.keys jobNamesById)
    cb = ComboBox.init [] -- With (Dict.keys jobNamesById)
    ( job, _ ) = Job.init
  in
    ( Model "rsync" "RSync" job []   -- Dict.empty  -- jobNamesById
      cb Util.Debug.init   -- "State: Just Started"  -- jobs_m
    , Cmd.none )


-- UPDATE

type Msg
  = NewJob
  | Rename String
  | JobMsg Job.Msg
  | ComboMsg ComboBox.Msg
  | DebugMsg Util.Debug.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model', msg' ) =
          case msg of
            NewJob ->
--              let
--                newOptions =
--                    [""] ++ List.map (\ (id, n) -> n) model.jobIdNames
--              in
                model !
                [ Cmd.map JobMsg <| Cmd.Extra.message <| Job.New model.name
----                , Cmd.Extra.message <| DebugMsg <| Util.Debug.Change <| "JobType.NewJob [" ++ model.name ++ "]"
--                , Cmd.Extra.message <| ComboMsg <| ComboBox.NewOptions newOptions
                ]

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
                _ = Debug.log "JobType.update:JobMsg" jmsg
                ( job', jmsg' ) = Job.update jmsg model.job

                otherJobIdNames =
--                    List.filter (\ (id,n) -> n /= job'.name) model.jobIdNames
                    List.filter (\ (id,n) -> id /= job'.id) model.jobIdNames
                jobNames = List.map (\ (id, n) -> n) otherJobIdNames
--                newOptions =
--                    case jmsg of
--                        Job.SaveSucceed newJob ->
--                            [newJob.name] ++
--                                List.filter (\ n -> n /= newJob.name) jobNames
--                        _ ->
--                            jobNames
                ( newOptionsMsgs, newJobIdNames, job'' ) =
                    case jmsg of
                        Job.SaveSucceed _ ->   -- newJob ->
                            ( [ Cmd.Extra.message
                                <| ComboMsg
                                <| ComboBox.NewOptions
--                                <| ( [newJob.name] ++
                                <| ( [job'.name] ++
--                                     List.filter (\ n -> n /= newJob.name) jobNames )
--                                     List.filter (\ n -> n /= job'.name)
                                     jobNames )
                            ]
                            , (job'.id, job'.name) :: otherJobIdNames  -- model.jobIdNames
                            , job' )
                        _ ->
                            ( [], model.jobIdNames, job' )
              in
                { model
                | job = job''
                , jobIdNames = newJobIdNames
                } !
                ( [ Cmd.map JobMsg jmsg'
--                , Cmd.Extra.message <| ComboMsg <| ComboBox.NewOptions newOptions
                ] ++ newOptionsMsgs )

            ComboMsg cbmsg ->
              updateCombo cbmsg model

        ( debug', dbgMsg' ) =
            let
                modelStr = toString
                    { id = model'.id, name = model'.name, jobs = model'.jobIdNames }
            in
                Util.Debug.update (Util.Debug.Change modelStr) model'.debug
--        _ = Debug.log "JobType.update.debug" debug'
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
        oldJobName = "old job " ++ (toString model.job.id) ++ ": " ++ (toString model.job.name)
--        _ = Debug.log "JobType.updateCombo" cbmsg
        ( cbb, cbmsg' ) = ComboBox.update cbmsg model.combo
        msg1 = Cmd.map ComboMsg ( -- Debug.log "combo"
                                 cbmsg' )
        (  -- action',
          nJob, msg2) =
          (
          case cbmsg of
            ComboBox.UpdateField s ->
--              let
----                _ = Debug.log "JobType.updateCombo:ComboBox.UpdateField" s
----                (job', jmsg) = Job.update (Job.Rename s) model.job
--                newJobName = "new job " ++ (toString job'.id) ++ ": " ++ (toString job'.name)
--                _ = Debug.log "JobType.updateCombo:ComboBox.UpdateField" newJobName
--              in
                ( -- "save " ++ oldJobName ++ " to " ++ newJobName,
--                 job'
                 model.job
--                , Cmd.map JobMsg jmsg )
                , Cmd.map JobMsg <| Cmd.batch
                    [ --jmsg
--                    , Cmd.Extra.message (Job.Save model.name model.job.id)
--                    ,
                      Cmd.Extra.message (Job.Rename s)
                    ]
                )
            ComboBox.Select s ->
              let
--                newJobName = "new job " ++ (  -- toString
--                    model.combo.current) ++ ": " ++ (  -- toString
--                    s)
--                _ = Debug.log "JobType.updateCombo:ComboBox.Select" s
                newJobId = findJobId s model
--                (newJob, jmsg) =
--                  Job.update (Job.Save model.name newJobId) model.job      -- <| findJobId s model
                _ = Debug.log "JobType.updateCombo:ComboBox.Select" (model.combo.current ++ " (" ++ model.job.id ++ ")" ++ " -->> " ++ s ++ " (" ++ newJobId ++ ")")
              in
                ( -- "save " ++ oldJobName ++ " and load " ++ newJobName,
--                  newJob
--                  job'
                  model.job
--                , Cmd.map JobMsg jmsg )

                , Cmd.map JobMsg <| Cmd.batch
                    [ --jmsg
--                    ,
                      Cmd.Extra.message (Job.Save model.name newJobId)   -- model.job.id)
                    ]
                 )

--            ComboBox.NewOptions sl ->
--                ( -- ("combo NewOptions " ++ (toString sl)),
--                  model.job
--                , Cmd.none )
            _ ->
                ( -- ("combo other " ++ (toString cbmsg)),
                  model.job
                , Debug.log ("combo other " ++ (toString cbmsg)) Cmd.none )
          )
      in
        { model
        | combo = cbb
--        , action = action'
        , job = nJob
        } !
        [ msg1
        , msg2
--        , Cmd.Extra.message <| DebugMsg <| Util.Debug.Change action'
        ]


findJobId : String -> Model -> String
findJobId jobName model =
    case List.filter (\(id, n) -> n == jobName) model.jobIdNames
        |> List.head
    of
        Nothing -> "---"
        Just (id, n) -> id

--findJobId : comparable -> Model -> String
--findJobId jobName model =
--    case Dict.values model.jobNamesById
--        |> List.filter (\n -> n == jobName)
--        |> List.head of
--        Nothing -> "---"
--        Just id -> id
--  case Dict.get jobName model.jobIdsByName of
--    Nothing -> "---"
--    Just id -> id


--saveLoadJob : String -> Model -> Cmd Msg
--saveLoadJob jobName model =
--    Job.saveLoadJobCall model.name model.job
--        |> Task.perform SaveFail SaveSucceed

--  let
----    url = "/jobs/RSync"
----    body_s =
----      -- initJob jobName model
----      model.job
----        |> encodeJob   -- model.job
----        |> Json.Encode.encode 2
----
----    postCall = Http.post decodeJobSaved url (Http.string body_s)
--    sljCall = saveLoadJobCall model.name model.job
--  in
----    Task.perform SaveFail SaveSucceed postCall
--    Task.perform SaveFail SaveSucceed sljCall


--itemsDecoder : Decode.Decoder (List String)
--itemsDecoder =
--  Decode.list Decode.string
--
--itemEncoder : String -> Encode.Value
--itemEncoder item =
--  Encode.object
--    [ ("item", Encode.string item) ]


--saveLoadJobCall : String -> Job.Model -> Task (HttpBuilder.Error String) (HttpBuilder.Response (Job.Model))
--saveLoadJobCall jobTypeName job =
--  HttpBuilder.post "/jobs/RSync"
--    |> withJsonBody (encodeJob jobTypeName job)
--    |> withHeader "Content-Type" "application/json"
--    |> withTimeout (10 * Time.second)
--    |> withCredentials
--    |> send (jsonReader decodeJob) stringReader
--
--
--{----------------------------------------------}
--decodeJob : Json.Decode.Decoder Job.Model
--decodeJob =
--    Json.Decode.succeed Job.Model
--        |: ("json_id"   := Json.Decode.string)
--        |: ("yaml_id"   := Json.Decode.string)
--        |: ("job_name"  := Json.Decode.string)
--        |: ("type_name" := Json.Decode.string)
--        |: ("cmd"       := Json.Decode.string)
--        |: ("root"      := Widget.Data.Json.decodeNode)
------------------------------------------------}
--
--encodeJob : String -> Job.Model -> Json.Encode.Value
--encodeJob jobTypeName job =
--    Json.Encode.object
----        [ ("json_id",   Json.Encode.string job.jsonId)
----        , ("yaml_id",   Json.Encode.string job.yamlId)
--        [ ("id",        Json.Encode.string job.id)
--        , ("job_name",  Json.Encode.string job.name)
--        , ("type_name", Json.Encode.string jobTypeName)
--        , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf job.root)
--        , ("root",      Widget.Data.Json.encodeNode job.root)
--        ]


{------------------------------------------------------------------
findJobId jobName model =
  case Dict.get jobName model.jobIdsByName of
    Nothing -> "---"
    Just id -> id

--saveLoadJob : Model -> String -> Job.Model
saveLoadJob allJobs oldJob newJobId =
  let
    job =
      case Dict.get newJobId --model.
          allJobs of
        Nothing -> oldJob   -- model.job
        Just j  -> j
  in
    job
------------------------------------------------------------------}


-- VIEW

view : Model -> Html Msg
view model =
      div []
      [ h2 [] [ text model.name ]
      , table []
        [ tr []
          [ td [] [ button [ Html.Events.onClick NewJob ] [ text "New"] ]
          , td [] [ button [] [ text "Clone"] ]
          , td [] [ button [] [ text "Save"] ]
          ]
        ]

--      view : List String -> String -> (String -> Msg) -> Model -> Html.Html Msg
--      view labels neutralEntry selectMsg model =
      , Html.App.map ComboMsg <| ComboBox.view ["Job"] "--" ComboBox.Select model.combo
      , Html.App.map JobMsg   <| Job.view model.name model.job
--      , Html.App.map DebugMsg <| Util.Debug.viewDbgStr "JobType" model.action model.debug
      , Html.App.map DebugMsg <| Util.Debug.view "JobType" model.debug
      ]
