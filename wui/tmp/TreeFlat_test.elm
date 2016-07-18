module TreeFlat_test exposing (..)

import Util.TreeFlat exposing (..)

import ElmTest       exposing (..)
import Html          exposing (..)


--myTest = defaultTest (5 `assertEqual` 5)

ul1Of : Record -> Html a
ul1Of tree =
  let
    flaTree = flatten tree
  in
    ul [] [
      li [] [ text <| toString tree ]
    , ul [] [
        li [] [ text <| toString <| flaTree ]
      , li [] [ text <| toString <| deflatten flaTree ]
      ]
    ]

ulOf : List Record -> Html a
ulOf treeList =
  ul [] ( List.concat (List.map liPair treeList) )

liPair : Record -> List (Html a)
liPair tree =
  let
    flaTree = flatten tree
  in
    [ li [] [ text <| toString tree ]
    --, ul [] [ li [] [ text <| toString <| flatten tree ] ]
    , ul [] [
        li [] [ text <| toString <| flaTree ]
      , li [] [ text <| toString <| deflatten flaTree ]
      ]
    ]

t1 : Record
t1 = Record "r1" (Node [t2, t3])
t2 : Record
t2 = Record "r2" (Node [t4, t5])
t3 : Record
t3 = Record "r3" (Node [])
t4 : Record
t4 = Record "r4" (Node [])
t5 : Record
t5 = Record "r5" (Node [t6, t7])
t6 : Record
t6 = Record "r6" (Node [])
t7 : Record
t7 = Record "r7" (Node [])

testSuite : String -> List Record -> Test
testSuite sname treeList =
  let
    reflat tree =
      ( tree, deflatten (flatten tree) )
    (otrees, ptrees) = List.unzip <| List.map reflat treeList
  in
    suite sname <| List.map defaultTest <| assertionList otrees ptrees

myTest : Test
myTest = testSuite "tree de-flatten" [t1, t2, t3, t4, t5, t6, t7]

main : Program Never
main =
  runSuiteHtml myTest
