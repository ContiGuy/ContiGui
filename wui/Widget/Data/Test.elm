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

module Widget.Data.Test exposing (..)

import Widget.Data.Type exposing (..)
import Widget.Data.Flat exposing (..)
import Widget.Data.Json exposing (..)


import Array
import List
import ElmTest          exposing (..)
import Html             exposing (..)


reflattenTestSuite : String -> List Node -> Test
reflattenTestSuite sname treeList =
  let
    reflat tree =
      ( tree, deflatten (flatten tree) )
    (otrees, ptrees) = List.unzip <| List.map reflat treeList
  in
    suite sname <| List.map defaultTest <| assertionList otrees ptrees

jsonizeTestSuite : String -> List Node -> Test
jsonizeTestSuite sname treeList =
  let
    deJsonizedWraps tree =
      case tree |> flatten |> wraps2json |> json2wraps of
        Err _ -> Array.empty
        Ok wraps_a -> wraps_a
    jsonize tree =
      ( tree, tree |> deJsonizedWraps |> deflatten )
    (otrees, ptrees) = List.unzip <| List.map jsonize treeList
  in
    suite sname <| List.map defaultTest <| assertionList otrees ptrees

myTest : List Node -> Test
myTest recs =
  suite "flatten + jsonize" [
    reflattenTestSuite "tree flatten" recs
  , jsonizeTestSuite "tree jsonize" recs
  ]

main : Html a
main =
  let
    recs = [t1, t2, t3, t4, t5, t6, t7]
  in
    ul [] [
      li [] [
        text <| stringRunner <| myTest recs
      ]
    , testRecsAsHtml recs
  ]


{---------------------------------------
---------------------------------------}

testRecsAsHtml : List Node -> Html a
testRecsAsHtml recs_l =
  let
    jsonize tree =
      tree |> flatten |> wraps2json
    dejsonize tree_json =
      case tree_json |> json2wraps of
        Err errMsg -> notFoundNode errMsg
        Ok wraps_a ->
          wraps_a |> deflatten

    deJsonizedWraps tree =
      case tree |> flatten |> wraps2json |> json2wraps of
        Err _ -> Array.empty
        Ok wraps_a -> wraps_a
    rejsonize tree =
      ( tree, tree |> deJsonizedWraps |> deflatten )
  in
    ul [] [
      li [] [ text "flatten"
            , ulOf flatten toString deflatten recs_l ]
    , li [] [ text "jsonize"
            , ulOf jsonize identity dejsonize recs_l ]
    ]


ulOf : (record -> flaTree) -> (flaTree -> String) -> (flaTree -> record) -> List record -> Html a
ulOf flatten flaToString deflatten recList =
  ul [] ( List.concat (List.map (liPair flatten flaToString deflatten) recList) )

liPair : (record -> flaTree) -> (flaTree -> String) -> (flaTree -> record) -> record -> List (Html a)
liPair flatten flaToString deflatten rec =
  let
    flaTree = flatten rec
  in
    [ li [] [ text <| toString rec ]
    , ul [] [
        li [] [ text <| flaToString <| flaTree ]
      , li [] [ text <| toString <| deflatten flaTree ]
      ]
    ]

sampleNode : id -> List Node -> Node
sampleNode id kids_l =
  Node (Record ("r" ++ (toString id))) (Kids kids_l)

t1 : Node
t1 = sampleNode 1 [t2, t3]
t2 : Node
t2 = sampleNode 2 [t4, t5]
t3 : Node
t3 = sampleNode 3 []
t4 : Node
t4 = sampleNode 4 []
t5 : Node
t5 = sampleNode 5 [t6, t7]
t6 : Node
t6 = sampleNode 6 []
t7 : Node
t7 = sampleNode 7 []

{---------------------------------------------------------
---------------------------------------------------------}

