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

{-------------------------------------------------
-------------------------------------------------}


{-------------------------------------------------
treeToJson : Int -> Node -> String
treeToJson indent node =
  JE.encode indent (jsonValueRec True node)

nodeToJson : Int -> Node -> String
nodeToJson indent node =
  JE.encode indent (jsonValueRec False node)

jsonValueRec : Bool -> Node -> JE.Value
jsonValueRec recurse node =
  let
    --(val, typ, boolState, strValue) =
    (val, typ) =
      case node.value of
        BoolValue b ->
          ( JE.bool b, "Bool" )
        StringValue s ->
          ( JE.string s, "String" )
        RootCmd ->
          ( JE.string node.label, "Root" )
        --Group isVertical showLabel ->
        Group isVertical ->
          ( JE.string node.label
          , if isVertical then
              "VerticalGroup"
            else
              "HorizontalGroup"
          )
        Switch sid ->
          ( JE.string sid, "Switch" )

    cmdlet = cmdOf node

    kids_l = kidsOf node
    extra =
      if recurse && List.length kids_l > 0 then
        [ ( "kids", JE.list ( List.map (jsonValueRec recurse) kids_l ) ) ]
      else
        []

    rootNode = JE.object ( [
        ( "id",          JE.string node.id )
      , ( "label",       JE.string node.label )
      , ( "description", JE.string node.descr )
      , ( "type",        JE.string typ )
      , ( "value",       val )
      , ( "cmdlet",      JE.string cmdlet )
      ] ++ extra )
  in
    rootNode

jobAsJson : Int -> String -> Node -> String
jobAsJson indent cfgName node =
  let
    rootNode = jsonValueRec True node
    job = JE.object ( [
        ( "name",          JE.string cfgName )
    ,   ( "root",          rootNode )
    ] )
  in
    JE.encode indent job
-------------------------------------------------}


{----------------------------------------------

nodeToJson : Int -> Node -> String
nodeToJson indent node =
  Json.Encode.encode indent (encodeNode node)


nodeAsHtmlLI : Node -> Html Msg
nodeAsHtmlLI node =
      li [] [ text ( nodeToJson 2 node )
      , kidsAsUL node
      ]

kidsAsUL : Node -> Html Msg
kidsAsUL node =
      ul [] ( List.map (\ k -> nodeAsHtmlLI k) ( kidsOf node ) )
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
    BoolValue _   ->   True
    StringValue _ ->   False
    RootCmd       ->   False
    Group _       ->   False
    Switch _      ->   True


-- VIEW

viewRoot : Node -> ( String, Html Msg )
viewRoot node =
  let
    kidsCont_l = List.map view ( kidsOf node )
    cont = div [] kidsCont_l
  in
    ( node.rec.label, cont)

view : Node -> Html Msg
view node =
  case node.rec.value of
    BoolValue _ ->
      -- node2TR node
      node2Table node

    StringValue _ ->
      -- node2TR node
      node2Table node

    RootCmd ->
      div [] ( [
        h2 [] [ a [ href "http://localhost:33333" ] [ text node.rec.label ] ]
      ] ++ ( List.map view ( kidsOf node ) ) )

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
              tr [] [ mkLabel showLabel nd, td [] [cont] ]
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



mkLabel : Bool -> Node -> Html Msg
mkLabel showLabel node =
  if showLabel then
    td [ title node.rec.descr ] [ text node.rec.label ]
  else
    td [ title ( node.rec.label ++ ": " ++ node.rec.descr ) ] []


kidsListOfTuples : Node -> List (Html Msg, Node, Bool)
kidsListOfTuples node =
  List.map viewTuple ( kidsOf node )

kidsTupleOfLists : Node -> (List (Html Msg), List (Html Msg))
kidsTupleOfLists node =
  --List.unzip (kidsListOfTuples node)
  let
    triple2htmlTuple (cont, kid, showLabel) =
      ( mkLabel showLabel kid, cont )
    tuples = List.map triple2htmlTuple (kidsListOfTuples node)
  in
    List.unzip (tuples)


{-----------------------------------------------}
node2Table : Node -> Html Msg
node2Table node =
  let
    (cont, nd, showLabel) = viewTuple node
    nTable node =
      table [ title (node.rec.label ++ " " ++ node.rec.id) ] [
        tr [] [ mkLabel showLabel node, td [] [cont] ]
      ]
  in
    case node.rec.value of
      BoolValue _ ->
        nTable node
      StringValue _ ->
        nTable node
      _ ->
        notImplemented node "node2Table"

-----------------------------------------------}




{-----------------------------------------------}
viewTuple : Node -> (Html Msg, Node, Bool)
viewTuple node =
  let
    (content, showLabel) =
      case node.rec.value of
        BoolValue flag ->
          ( input [ type' "checkbox", checked flag, onCheck (editBool node.rec.id) ] []
          , True
          )
        StringValue str ->
          ( input [ type' "text", value str, onInput (editString node.rec.id) ] []
          , True
          )
        RootCmd ->
          ( view node
          , True
          )

        --Group isVertical showLabel ->
        Group isVertical ->
          ( view node
          , True
          )

        Switch sid ->
          -- input [ type' "radio", checked False
          -- ] []
          ( view node
          --, ""  --  node.label
          --, node.label
          , False
          )
  in
    ( content, node, showLabel )
-----------------------------------------------}


editBool : Id -> Bool -> Msg
editBool id b =
  Modify id (BoolValue b)

editString : Id -> String -> Msg
editString id s =
  Modify id (StringValue s)

selectSwitch : Id -> Id -> Msg
selectSwitch sid kid =
  Modify sid (Switch kid)

notImplemented : Node -> String -> Html Msg
notImplemented node errDesr =
  div [ {-color "red"-} ] [ text ( "ERROR: " ++ errDesr ++ " NOT IMPLEMENTED: " ++ node.rec.label ++ ": " ++ ( toString node.rec.value ) ) ]

{----------------------------------------------------------------
------------------------------------------------------------------}
