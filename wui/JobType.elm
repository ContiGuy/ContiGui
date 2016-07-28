module JobType exposing (..)

import Html            exposing (..)
import Html.Attributes
import Html.App
import Html.Events
import Http
import Task
import Cmd.Extra
import Json.Encode     -- exposing ((:=))
import Json.Decode          exposing ((:=))
import Json.Decode.Extra    exposing ((|:))

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
  , jobIdNames   : List (String, String)
  , combo        : ComboBox.Model
  , debug        : Util.Debug.Model
  }

init : ( Model, Cmd Msg )
init =
--  let
--    cb = ComboBox.init []
----    ( job, _ ) = Job.init
--  in
    ( Model "rsync" "RSync" defaultJob  -- job
        [] emptyComboBox   -- cb
        Util.Debug.init
--    , Cmd.none
    , loadJobs
    )

defaultJob : Job.Model
defaultJob =
    fst Job.init

emptyComboBox : ComboBox.Model
emptyComboBox =
    ComboBox.init []


-- UPDATE

type Msg
  = NewJob
  | Rename String
  | JobMsg Job.Msg
  | ComboMsg ComboBox.Msg
  | DebugMsg Util.Debug.Msg
  | LoadJobs
  | LoadJobsFail Http.Error
  | LoadJobsSucceed Model  --  (List Model)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model', msg' ) =
          case msg of
            NewJob ->
                model ! [ Cmd.map JobMsg <| Cmd.Extra.message <| Job.New model.name ]

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
--                _ = Debug.log ("JobType.update:JobMsg " ++ (toString model.jobIdNames)) jmsg
                _ = Debug.log ("JobType.update:JobMsg") jmsg
                ( job', jmsg' ) = Job.update jmsg model.job

--                otherId (id, n) =
--                    id /= model.job.id && id /= job'.id
--
--                validId (id, n) =
--                    id /= ""
--
--                otherJobIdNames =
--                    List.filter otherId model.jobIdNames
--                _ = Debug.log "JobType.update:JobMsg.otherJobIdNames" otherJobIdNames
--
--                newJobIdNames =
--                    if model.job.id == job'.id then
--                        (job'.id, job'.name) :: otherJobIdNames
--                    else
--                        (job'.id, job'.name)
--                            :: (model.job.id, model.job.name)
--                            :: otherJobIdNames
--                _ = Debug.log "JobType.update:JobMsg.newJobIdNames" newJobIdNames
--
--                validJobIdNames =
--                    List.filter validId newJobIdNames
--                _ = Debug.log "JobType.update:JobMsg.validJobIdNames" validJobIdNames

                validJobIdNames =
                    newJobIdNames model job'

                newJobNames =
                    List.map (\ (id, n) -> n) validJobIdNames

                newOptionsMsgs =
                    case jmsg of
                        Job.SaveSucceed _ ->
                            [ Cmd.Extra.message <| ComboMsg <| ComboBox.NewOptions newJobNames ]
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
--                model ! [ loadJobs model ]
                model ! [ loadJobs ]

--                let
--                  loadJobsCmdMsg =
--                    loadJobs model
--                in
--                  { model
--                  | output = "JobType.LoadJobs ..."
--                  , lastOk = Just "loading jobs ..."
--                  }
--                  ! [ loadJobsCmdMsg ]   -- Cmd.batch [ saveCmdMsg, xCmdMsg ] ]

            LoadJobsFail err ->  -- Http.Error
                let
                    _ = Debug.log "JobType.LoadJobsFail" err
                in
                    model ! []

            LoadJobsSucceed jobType ->  -- (List Model)
                let
                    _ = Debug.log "JobType.LoadJobsSucceed" jobType

                    loadedJobNames =
                        List.map (\ (id, n) -> n) jobType.jobIdNames
                    _ = Debug.log "JobType.LoadJobsSucceed:loadedJobNames" loadedJobNames

                    jobLoadMsgs =
                        case List.head jobType.jobIdNames of
                            Nothing -> []
                            Just (jobId, jobName ) ->
                                [ Cmd.Extra.message <| JobMsg <| Job.Load jobType.name jobId
                                ]
                in
                    jobType !
                    ( [ Cmd.Extra.message <| ComboMsg <| ComboBox.NewOptions loadedJobNames
                    ] ++ jobLoadMsgs )


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
        oldJobName = "old job " ++ (toString model.job.id) ++ ": " ++ (toString model.job.name)
        ( cbb, cbmsg' ) = ComboBox.update cbmsg model.combo
        msg1 = Cmd.map ComboMsg cbmsg'
        (
          nJob, msg2) =
          (
          case cbmsg of
            ComboBox.UpdateField s ->
                ( model.job
                , Cmd.map JobMsg <| Cmd.batch [ Cmd.Extra.message <| Job.Rename s ]
                )
            ComboBox.Select s ->
              let
                newJobId = findJobId s model
                _ = Debug.log "JobType.updateCombo:ComboBox.Select" (model.combo.current ++ " (" ++ model.job.id ++ ")" ++ " -->> " ++ s ++ " (" ++ newJobId ++ ")")
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
        [ msg1
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
          [ td [] [ button [ Html.Events.onClick NewJob ] [ text "New"] ]
          , td [] [ button [ Html.Attributes.disabled True ] [ text "Clone"] ]
--          , td [] [ button [ Html.Attributes.disabled True ] [ text "Save"] ]
          , td [] [ button [ Html.Events.onBlur <| JobMsg <| Job.Save model.name "" ] [ text "Save"] ]
          ]
        ]

--      , div [ Html.Events.onBlur <| JobMsg <| Job.Save model.name "" ]
--        [ Html.App.map ComboMsg <| ComboBox.view ["Job"] "--" ComboBox.Select model.combo
--        ]
      , Html.App.map ComboMsg <| ComboBox.view ["Job"] "--" ComboBox.Select model.combo
      , Html.App.map JobMsg   <| Job.view model.name model.job
      , Html.App.map DebugMsg <| Util.Debug.view "JobType" model.debug
      ]


-- Helpers


newJobIdNames : Model -> Job.Model -> List ( String, String )
--    -> { d | name : a, id : String }
--    -> List ( String, a )
newJobIdNames model job' =
  let
    otherId (id, n) =
        id /= model.job.id && id /= job'.id

    validId (id, n) =
        id /= ""

    otherJobIdNames =
        List.filter otherId model.jobIdNames
    _ = Debug.log "JobType.newJobIdNames:otherJobIdNames" otherJobIdNames

    newJobIdNames =
        if model.job.id == job'.id then
            (job'.id, job'.name) :: otherJobIdNames
        else
            (job'.id, job'.name)
                :: (model.job.id, model.job.name)
                :: otherJobIdNames
    _ = Debug.log "JobType.newJobIdNames:newJobIdNames" newJobIdNames

    validJobIdNames =
        List.filter validId newJobIdNames
    _ = Debug.log "JobType.newJobIdNames:validJobIdNames" validJobIdNames
  in
    validJobIdNames



--loadJobs : Model -> Cmd Msg
--loadJobs model =
loadJobs : Cmd Msg
loadJobs =
  let
    url = Debug.log "loading jobs from" "/jobs/RSync"
--    httpCall = Http.get decodeJobTypes url
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
        |: ( Json.Decode.Extra.withDefault defaultJob ("job"  := Job.decode) )
--        |: Json.Decode.null    defaultJob
        |: ("jobs"          := decodeJobIdNamesList)
        |: ( Json.Decode.Extra.withDefault emptyComboBox ("combo"  := Json.Decode.null    emptyComboBox) )
--        |: Json.Decode.null    emptyComboBox
        |: ( Json.Decode.Extra.withDefault Util.Debug.init ("debug"  := Json.Decode.null    Util.Debug.init) )
--        |: Json.Decode.null    Util.Debug.init
--        |: ( Json.Decode.Extra.withDefault "" ("job_name"  := Json.Decode.string) )
--        |: ("type_name" := Json.Decode.string)
--        |: ("root"      := Widget.Data.Json.decodeNode)
--        |: ( Json.Decode.Extra.withDefault Util.Debug.init ("debug"  := Util.Debug.decode)  )


decodeJobIdNamesList : Json.Decode.Decoder (List ( String, String ))
decodeJobIdNamesList =
    Json.Decode.map jobHeadList2IdNameTupleList (Json.Decode.list decodeJobHead)

jobHeadList2IdNameTupleList : List JobHead -> List (String, String)
jobHeadList2IdNameTupleList jobHeadList =
    List.map (\ jh -> (jh.id, jh.name) ) jobHeadList

type alias JobHead =
    { id       : String
    , name     : String
    , typeName : String
    }

-- decode only the head of a single job
decodeJobHead : Json.Decode.Decoder JobHead
decodeJobHead =
    Json.Decode.succeed JobHead
        |: ("job_id"        := Json.Decode.string)
        |: ("job_name"      := Json.Decode.string)
        |: ("type_name"     := Json.Decode.string)
----------------------------------------------}




----encodeJob : (node -> Json.Encode.Value) -> String -> Model -> Json.Encode.Value
----encodeJob encodeNode jobTypeName job =
--encode : String -> Model -> Json.Encode.Value
--encode jobTypeName model =
--    encodeX jobTypeName model [
--        ("root",      Widget.Data.Json.encodeNode model.node)
--    ]
--
----    Json.Encode.object
------        [ ("json_id",   Json.Encode.string job.jsonId)
------        , ("yaml_id",   Json.Encode.string job.yamlId)
----        [ ("id",        Json.Encode.string job.id)
----        , ("job_name",  Json.Encode.string job.name)
----        , ("type_name", Json.Encode.string jobTypeName)
----        , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf job.node)
------        , ("root",      encodeNode job.node)
----        , ("root",      Widget.Data.Json.encodeNode job.node)
----        ]
--
----encodeX : String -> Model -> Json.Encode.Value
--encodeX
--    : String
--    -> Model  --  { a | id : String, name : String, node : Node }
--    -> List ( String, Json.Encode.Value )
--    -> Json.Encode.Value
--encodeX jobTypeName model addFields =
--    encodeNewJobOfType jobTypeName
--    ( [ ("job_id",    Json.Encode.string model.id)
--      , ("job_name",  Json.Encode.string model.name)
--      , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf model.node)
--      , ("debug",     Util.Debug.encode model.debug)
--      ] ++ addFields
--    )
----    Json.Encode.object (
------        [ ("json_id",   Json.Encode.string job.jsonId)
------        , ("yaml_id",   Json.Encode.string job.yamlId)
----        [ ("job_id",    Json.Encode.string model.id)
----        , ("job_name",  Json.Encode.string model.name)
----        , ("type_name", Json.Encode.string jobTypeName)
----        , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf model.node)
------        , ("root",      encodeNode job.node)
------        , ("root",      Widget.Data.Json.encodeNode job.node)
----        , ("debug",     Util.Debug.encode model.debug)
----        ] ++ addFields
----      )
--
--encodeNewJobOfType
--    : String
--    -> List ( String, Json.Encode.Value )
--    -> Json.Encode.Value
--encodeNewJobOfType jobTypeName jobFields =
--    Json.Encode.object (
--        [ ("type_name", Json.Encode.string jobTypeName)
--        ] ++ jobFields
--      )
------        [ ("json_id",   Json.Encode.string job.jsonId)
------        , ("yaml_id",   Json.Encode.string job.yamlId)
----        [ ("job_id",    Json.Encode.string model.id)
----        , ("job_name",  Json.Encode.string model.name)
----        , ("type_name", Json.Encode.string jobTypeName)
----        , ("cmd",       Json.Encode.string <| Widget.Gen.cmdOf model.node)
------        , ("root",      encodeNode job.node)
------        , ("root",      Widget.Data.Json.encodeNode job.node)
----        , ("debug",     Util.Debug.encode model.debug)
----        ] ++ addFields
----      )
