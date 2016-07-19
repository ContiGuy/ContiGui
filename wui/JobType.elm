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

module JobType exposing (..)

import Json.Encode
import Json.Decode       exposing ((:=))
import Json.Decode.Extra exposing ((|:))

--import Widget
import Widget.Data.Type exposing (..)
import Widget.Data.Json  -- exposing (..)

-- MODEL

type alias JobTypes =
    { jobTypes : List JobType
    }

{----------------------------------------------}
{----------------------------------------------
----------------------------------------------}
type alias JobType =
    { jobs : List Job
    , id   : String
    , name : String
    }

type alias Job =
    { jsonId   : String
    , yamlId   : String
    , name     : String
    , typeName : String
    , root     : Widget.Data.Type.Node
    }


{----------------------------------------------
init : String -> JobType
init jobTypeName =
  JobType [
    Job "0-json" "0-yaml" "--"      jobTypeName
  , Job "1-json" "1-yaml" "default" jobTypeName
  , Job "2-json" "2-yaml" "hra"     jobTypeName
  , Job "3-json" "3-yaml" "kati"    jobTypeName
  ] "jt5" jobTypeName
----------------------------------------------}


{----------------------------------------------}
decodeJobTypes : Json.Decode.Decoder JobTypes
decodeJobTypes =
    Json.Decode.succeed JobTypes
        |: ("job_types" := ( Json.Decode.list decodeJobType ) )
{----------------------------------------------}


--  Json.Decode.oneOf [
--    Json.Decode.object1 BoolValue ( "bool" := Json.Decode.bool )
--  , Json.Decode.object1 StringValue ( "string" := Json.Decode.string )

decodeJobType : Json.Decode.Decoder JobType
decodeJobType =
    Json.Decode.succeed JobType
--        |: ("jobs" := Json.Decode.oneOf [
--                        decodeMaybeJobList ( Json.Decode.maybe ( Json.Decode.list decodeJob ) )
--                      , Json.Decode.null []
--                      ] )

--        |: ("jobs" := decodeMaybeJobList ( Json.Decode.maybe ( Json.Decode.list decodeJob ) ) )
--        |: ( Json.Decode.maybe ("jobs" := Json.Decode.list decodeJob) )
        |: ("jobs" := Json.Decode.list decodeJob)

        |: ("id"   := Json.Decode.string)
        |: ("name" := Json.Decode.string)
{----------------------------------------------}


decodeMaybeJobList
    : Json.Decode.Decoder (Maybe (List a))
    -> Json.Decode.Decoder (List a)
decodeMaybeJobList jobListDecoder =
  let
    unwrapMaybeJobList maybeJL =
      case maybeJL of
        Just jobList -> jobList
        Nothing -> []
  in
    Json.Decode.map unwrapMaybeJobList jobListDecoder

encodeJobTypes : JobTypes -> Json.Encode.Value
encodeJobTypes record =
    Json.Encode.object
        [ ("job_types", Json.Encode.list ( List.map encodeJobType record.jobTypes ) ) ]

{----------------------------------------------}

encodeJobType : JobType -> Json.Encode.Value
encodeJobType jobType =
    Json.Encode.object
        [ ("jobs", Json.Encode.list   <| List.map encodeJob jobType.jobs)
        , ("id",   Json.Encode.string    jobType.id)
        , ("name", Json.Encode.string    jobType.name)
        ]
{----------------------------------------------}

decodeJob : Json.Decode.Decoder Job
decodeJob =
    Json.Decode.succeed Job
        |: ("json_id"   := Json.Decode.string)
        |: ("yaml_id"   := Json.Decode.string)
        |: ("name"      := Json.Decode.string)
        |: ("type_name" := Json.Decode.string)
        |: ("root"      := Widget.Data.Json.decodeNode)

encodeJob : Job -> Json.Encode.Value
encodeJob job =
    Json.Encode.object
        [ ("json_id",   Json.Encode.string job.jsonId)
        , ("yaml_id",   Json.Encode.string job.yamlId)
        , ("name",      Json.Encode.string job.name)
        , ("type_name", Json.Encode.string job.typeName)
        , ("root",      Widget.Data.Json.encodeNode job.root)
        ]

{----------------------------------------------


-- UPDATE

type Msg =
  Load

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  table [] [ tr [] [
    td [] [ label [] [ text "Configuration" ] ]
  , td [] [ select []
      ( List.map (\ j -> option [] [ text j.name ] ) model.jobs ) ]
  ] ]

----------------------------------------------}
