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

module JsonTest exposing (..)

import Html              exposing (..)
import Json.Decode       exposing (..)  --  (decodeString, (:=), object1, object2, string, int, list)
import Json.Decode.Extra exposing (withDefault, lazy)

type alias Record =
  { id : Int
  --, tree : Tree
  }

--emptyRec =
--  Record 0   -- Leaf

decodeRecord =
  object1 Record
    ( withDefault -33 ( "id" := int ) )
    -- (withDefault Leaf ( "tree" := decodeTree ) )

type Tree
    = Leaf
    | Node Record ( List Tree )

{-| Json Decoder for a recursive data structure
-}
decodeTree : Json.Decode.Decoder Tree
decodeTree =
  object2
    Node
    (map Record (withDefault -33 ("id" := int)))
    (withDefault []
        ("kids" := lazy (\_ -> -- map Node
           (list decodeTree))) )
        -- !! -- ("kids" := treeNode))

decodeStr2LI : Json.Decode.Decoder a -> String -> Html b
decodeStr2LI decoder s =
    li [] [
      s ++ "  >>>  " ++ (decodeString decoder s |> toString)
      |> text
    ]

decodeTree2Text2LI : String -> Html a
decodeTree2Text2LI s =
    decodeStr2LI decodeTree s

decodeRecord2Text2LI : String -> Html a
decodeRecord2Text2LI s =
    decodeStr2LI decodeRecord s

recordsLI =
  li [] [ text "Records:", ul [] [
      decodeRecord2Text2LI """{"id":3, "tree":{"name": "joe"}}"""
  ] ]

treesLI =
  li [] [ text "Trees:", ul [] [
      text "OK:"
    , decodeTree2Text2LI """{"kids": [{"kids":[]},{"kids":[{"kids":[]}]}]}"""
    , decodeTree2Text2LI """{"name": "joe", "kids":[]}"""
    , text "FAIL:"
    , decodeTree2Text2LI """{"name": "joe"}"""
    , decodeTree2Text2LI """{"name": "joe", "kids": {"name":"jim"}}"""
    , decodeTree2Text2LI """{"kids": [{"kids":[]}]}"""
    , decodeTree2Text2LI """{"name": "joe"}"""
    ] ]


main : Html a
main =
    ul [] [
      treesLI
    , recordsLI
    ]
