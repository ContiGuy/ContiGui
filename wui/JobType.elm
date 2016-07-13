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

--import Widget as W exposing (..)

--import Html exposing (..)
--import Html.Events exposing (..)
--import Html.Attributes exposing (..)
--import Html.App
import Json.Encode                               --as JE
import Json.Decode exposing ((:=))  --, (|:)) -- as JD
import Json.Decode.Extra exposing ((|:)) -- as JD
--import Regex as RX   -- exposing (regex) as RX
--import String exposing (..)
--import Dict   -- as Di  --  exposing (..)

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
    --, node : W.Node
    --, cfgName      : String
    --, tmpCfgName   : String
    }

type alias Job =
    { -- versions : List String
    --, 
      jsonId   : String
    , yamlId   : String
    , name     : String
    , typeName : String
    }


{----------------------------------------------
----------------------------------------------}

init : String -> JobType
init jobTypeName =
  JobType [
    Job "0-json" "0-yaml" "Create New" jobTypeName
  , Job "1-json" "1-yaml" "default" jobTypeName
  , Job "2-json" "2-yaml" "hra" jobTypeName
  , Job "3-json" "3-yaml" "kati" jobTypeName
  ] "jt5" jobTypeName  -- "Unknown Job Type"

{----------------------------------------------}
decodeJobTypes : Json.Decode.Decoder JobTypes
decodeJobTypes =
    Json.Decode.succeed JobTypes
        |: ("job_types" := ( Json.Decode.list decodeJobType ) )
{----------------------------------------------}

decodeJobType : Json.Decode.Decoder JobType
decodeJobType =
    Json.Decode.succeed JobType
        |: ("jobs" := Json.Decode.list decodeJob)
        |: ("id"   := Json.Decode.string)
        |: ("name" := Json.Decode.string)
{----------------------------------------------}

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
        -- |: ("versions" := Json.Decode.list Json.Decode.string)
        |: ("json_id"  := Json.Decode.string)
        |: ("yaml_id"  := Json.Decode.string)
        |: ("name"     := Json.Decode.string)
        |: ("typeName" := Json.Decode.string)

encodeJob : Job -> Json.Encode.Value
encodeJob job =
    Json.Encode.object
        [ -- ("versions", Json.Encode.list <| Json.Encode.list (Json.Encode.string job.versions))
          ("json_id",  Json.Encode.string job.jsonId)
        , ("yaml_id",  Json.Encode.string job.yamlId)
        , ("name",     Json.Encode.string job.name)
        , ("typeName", Json.Encode.string job.typeName)
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
