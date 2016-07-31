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

module Widget exposing (..)
{-------------------------------------------------------
    Node, Msg, Id
  , aRoot, aVertical, aHorizontal, aSwitch, aBool, aBoolX, aBooT, aString
  --, Formatter
  , fmtList, fmtById   -- , fmtBool

  , update
  , view, viewRoot
  --, treeToJson
  , nodeToJson  --, toJson
  --, jobAsJson

  , nodeAsHtmlLI, cmdOf, kidsOf
  )
-------------------------------------------------------}

import Widget.Data.Type exposing (..)
import Widget.Gen       exposing (..)



import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
--import Html.App
--import Json.Encode
--import Json.Decode       exposing ((:=))
--import Json.Decode.Extra exposing ((|:))
--import Regex as RX
--import String exposing (..)
--import Dict


-- MODEL

--{----------------------------------------------
--
--nodeToJson : Int -> Node -> String
--nodeToJson indent node =
--  Json.Encode.encode indent (encodeNode node)
--
--
--nodeAsHtmlLI : Node -> Html Msg
--nodeAsHtmlLI node =
--      li [] [ text ( nodeToJson 2 node )
--      , kidsAsUL node
--      ]
--
--kidsAsUL : Node -> Html Msg
--kidsAsUL node =
--      ul [] ( List.map (\ k -> nodeAsHtmlLI k) ( kidsOf node ) )
----------------------------------------------
----------------------------------------------}


{----------------------------------------
----------------------------------------}



-- UPDATE

type Msg =
    Modify Id Value

--update : Msg -> Node -> ( Node, Cmd Msg, Bool )
update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
  mapUpdate (updateSingleNode msg) node

--updateSingleNode : Msg -> Node -> ( Node, Cmd Msg, Bool )
updateSingleNode : Msg -> Node -> ( Node, Cmd Msg )
updateSingleNode msg node =
  case msg of
    Modify id val ->
      let
        value = val
      in
        if id == node.rec.id then
          let
            rec0 = node.rec
          in
            ( { node
              | rec = { rec0
                      | value = Debug.log ( "update " ++ node.rec.label ) value
                      }
              }
            , Cmd.none
            --, valueUpdateRequiresSaving value
            )
        else
          ( node, Cmd.none
          --, False
          )


{-----------------------------------------------------------}
--mapUpdate : (Node -> (Node, Cmd a, Bool)) -> Node -> (Node, Cmd a, Bool)
mapUpdate : (Node -> (Node, Cmd a)) -> Node -> (Node, Cmd a)
mapUpdate f node =
  let
--    ( newNode, cmd, saveNeeded )  = f node
    ( newNode, cmd )  = f node
--    ( newKids, cmds, savesNeeded_l ) = unzip3 ( List.map (mapUpdate f) (kidsOf node) )
    ( newKids, cmds ) = List.unzip ( List.map (mapUpdate f) (kidsOf node) )
  in
    ( replaceKids newNode newKids
    , Cmd.batch ( cmd :: cmds )
    --, List.foldl (||) False (saveNeeded :: savesNeeded_l)
--    , anyIsTrue <| saveNeeded :: savesNeeded_l
    )
-----------------------------------------------------------}

anyIsTrue : List Bool -> Bool
anyIsTrue bool_l =
  List.foldl (||) False bool_l

unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 typle3_l =
    -- foldl : (a -> b -> b) -> b -> List a -> b
    List.foldl append3Tuple ([], [], []) typle3_l

append3Tuple
    : ( a, b, c )
    -> ( List a, List b, List c )
    -> ( List a, List b, List c )
append3Tuple tuple3 listsTuple =
  case listsTuple of
    (x_l, y_l, z_l) ->
      case tuple3 of
        (x, y, z) ->
          (x_l ++ [x], y_l ++ [y], z_l ++ [z])


valueUpdateRequiresSaving : Value -> Bool
valueUpdateRequiresSaving value =
  case value of
    BoolValue _     ->   True
    StringValue _ _ ->   False
--    RootCmd       ->   False
    Group _         ->   False
    Switch _        ->   True


-- VIEW

viewRoot : Node -> ( String, Html Msg )
viewRoot node =
  let
    kidsCont_l = List.map view ( kidsOf node )
    cont = div [] kidsCont_l
  in
    ( node.rec.label, cont)

viewRecord : Html Msg -> Record -> Html Msg
viewRecord defaultHtml rec =
  case rec.value of
    BoolValue _ ->
--      node2Table node
      record2Table rec

    StringValue _ _ ->
--      node2Table node
      record2Table rec

    _ ->
        defaultHtml

view : Node -> Html Msg
view node =
  case node.rec.value of
    BoolValue _ ->
--      node2Table node
      record2Table node.rec

    StringValue _ _ ->
--      node2Table node
      record2Table node.rec

--    RootCmd ->
--      div [] ( [
--        h2 [] [ a [ href "http://localhost:33333" ] [ text node.rec.label ] ]
--      ] ++ ( List.map view ( kidsOf node ) ) )

    -- Group isVertical ->
    Group orientation ->
      viewGroup orientation node
      {-------------------------------------------------------------
      -------------------------------------------------------------}


    Switch sid ->
      {-------------------------------------------}
      let
        kids_l = kidsOf node
        kidsIdsAndLabels_l = List.map (\ k -> (k.rec.id, k.rec.label) ) kids_l
        mkRadioTR (id, lbl) =
          tr [] [
            td [] [ label [] [ text lbl ] ]
          , td [] [ input [ type' "radio", value id, name node.rec.id
                    , checked (id == sid)
                    , onClick (selectSwitch node.rec.id id)
                    ] [] ]
          ]
        kidsAsRadioTRs_l = List.map mkRadioTR kidsIdsAndLabels_l
        switchBoard = tr [] [ th [] [
            text node.rec.label
          ]] :: kidsAsRadioTRs_l

        selectedKidTR =
          case (getSelectedKid sid node) of
            Nothing ->
              tr [ title "please select one switch option" ] [ td [] [
                text ("(please select one of the options for "
                  ++ node.rec.label ++ ")")
              ] ]
            Just kid ->
              view kid
      in
        tr [ title (node.rec.label ++ " " ++ node.rec.id) ] [
          td [] [ table [] switchBoard ]
        , td [] [ table [] [ selectedKidTR ] ]
        ]
      -------------------------------------------}

viewGroup : Orientation -> Node -> Html Msg
viewGroup orientation node =
  let
    showLabelXX = True
    toolTip =
      title ( node.rec.label ++ " # " ++ (toString node.rec.value) )
    tableRows =
      case orientation of
        --Widget.Data.
        Vertical ->
          let
            -- one row
            row (cont, nd, showLabel) =
              tr [] [ mkLabel showLabel nd.rec, td [] [cont] ]
          in
            -- all rows
              ( List.map row (kidsListOfTuples node) )

        --Widget.Data.
        Horizontal ->
          -- horizontal
          let
            (labels, conts) = kidsTupleOfLists node
            tdOf cont = td [] [ cont ]
            row1 = tr [] ( labels )
            row2 = tr [] ( List.map tdOf conts )
          in
            [ row1, row2 ]

        --Widget.Data.
        Disoriented ->
          [ text "DISORIENTED" ]
  in
    table [ toolTip ] tableRows



--mkLabel' : Bool -> Node -> Html Msg
--mkLabel' showLabel node =
--    let
--        em' styles lbl =
--            em
--                ( if List.length styles > 0 then
--                    [ Html.Attributes.style styles ]
--                  else []
--                )
--                [ text lbl ]
--        (sfx, txt) =
--            case node.rec.value of
--                StringValue s required ->
--                    if required then
--                        (" *"
--                        , em' ( if s == "" then [("color", "red")] else [] )
--                        )
--                    else
--                        ("", text)
--                _ ->
--                    ("", text)
--    in
--      if showLabel then
--        td [ title node.rec.descr ] [ txt <| node.rec.label ++ sfx ]
--      else
--        td [ title ( node.rec.label ++ sfx ++ " : " ++ node.rec.descr ) ] []

mkLabel : Bool -> Record -> Html Msg
mkLabel showLabel rec =
    let
        em' styles lbl =
            em
                ( if List.length styles > 0 then
                    [ Html.Attributes.style styles ]
                  else []
                )
                [ text lbl ]
        (sfx, txt) =
            case rec.value of
                StringValue s required ->
                    if required then
                        (" *"
                        , em' ( if s == "" then [("color", "red")] else [] )
                        )
                    else
                        ("", text)
                _ ->
                    ("", text)
    in
      if showLabel then
        td [ title rec.descr ] [ txt <| rec.label ++ sfx ]
      else
        td [ title ( rec.label ++ sfx ++ " : " ++ rec.descr ) ] []


kidsListOfTuples : Node -> List (Html Msg, Node, Bool)
kidsListOfTuples node =
  List.map viewTuple ( kidsOf node )

kidsTupleOfLists : Node -> (List (Html Msg), List (Html Msg))
kidsTupleOfLists node =
  --List.unzip (kidsListOfTuples node)
  let
    triple2htmlTuple (cont, kid, showLabel) =
      ( mkLabel showLabel kid.rec, cont )
    tuples = List.map triple2htmlTuple (kidsListOfTuples node)
  in
    List.unzip (tuples)


{-----------------------------------------------}
--node2Table : Node -> Html Msg
--node2Table node =
--  let
--    (cont, nd, showLabel) = viewTuple node
--    nTable node =
--      table [ title (node.rec.label ++ " " ++ node.rec.id) ] [
--        tr [] [ mkLabel' showLabel node, td [] [cont] ]
--      ]
--  in
--    case node.rec.value of
--      BoolValue _ ->
--        nTable node
--      StringValue _ _ ->
--        nTable node
--      _ ->
--        notImplemented' node "node2Table"

record2Table : Record -> Html Msg
record2Table rec =
  let
    (cont, showLabel) =
        viewTupleOfRecord rec
    nTable rec =
      table [ title (rec.label ++ " " ++ rec.id) ]
      [ tr [] [ mkLabel showLabel rec, td [] [cont] ]
      ]
  in
    case rec.value of
      BoolValue _ ->
        nTable rec
      StringValue _ _ ->
        nTable rec
      _ ->
        notImplemented rec "node2Table"

-----------------------------------------------}




{-----------------------------------------------}
viewTupleOfRecord : Record -> (Html Msg, Bool)
viewTupleOfRecord rec =
  let
    (content, showLabel) =
      case rec.value of
        BoolValue flag ->
          ( input [ type' "checkbox", checked flag, onCheck (editBool rec.id) ] []
          , True
          )
        StringValue str _ ->
          ( input [ type' "text", value str, onInput (editString rec.id) ] []
          , True
          )
        _ ->
          ( notImplemented rec "node2Table"
          , False
          )
  in
    ( content, showLabel )


viewTuple : Node -> (Html Msg, Node, Bool)
viewTuple node =
  let
    (content, showLabel) =
      case node.rec.value of
--        BoolValue flag ->
--          ( input [ type' "checkbox", checked flag, onCheck (editBool node.rec.id) ] []
--          , True
--          )
--        StringValue str _ ->
--          ( input [ type' "text", value str, onInput (editString node.rec.id) ] []
--          , True
--          )

--        RootCmd ->
--          ( view node
--          , True
--          )

        --Group isVertical showLabel ->
--        Group isVertical ->
        Group _ ->
          ( view node
          , True
          )

--        Switch sid ->
        Switch _ ->
          -- input [ type' "radio", checked False
          -- ] []
          ( view node
          --, ""  --  node.label
          --, node.label
          , False
          )

        _ ->
            viewTupleOfRecord node.rec
  in
    ( content, node, showLabel )
-----------------------------------------------}


editBool : Id -> Bool -> Msg
editBool id b =
  Modify id (BoolValue b)

editString : Id -> String -> Msg
editString id s =
  Modify id (StringValue s True)

selectSwitch : Id -> Id -> Msg
selectSwitch sid kid =
  Modify sid (Switch kid)

--notImplemented' : Node -> String -> Html Msg
--notImplemented' node errDesr =
--  div [ {-color "red"-} ] [ text ( "ERROR: " ++ errDesr ++ " NOT IMPLEMENTED: " ++ node.rec.label ++ ": " ++ ( toString node.rec.value ) ) ]

notImplemented : Record -> String -> Html Msg
notImplemented rec errDesr =
  div [ {-color "red"-} ]
  [ text ( "ERROR: " ++ errDesr ++ " NOT IMPLEMENTED: "
           ++ rec.label ++ ": " ++ ( toString rec.value ) )
  ]

{----------------------------------------------------------------
------------------------------------------------------------------}
