module Widget.Gen exposing (..)

import Widget.Data.Type exposing (..)
import Widget.Data.Flat   -- exposing (..)

import List
import String -- exposing (contains)
import Regex  -- as RX
import Dict
import Array



cmdOf : Node -> String
cmdOf node =
  let
    selectedId node =
      case node.value of
        Switch toKidId ->
          toKidId
        _ ->
          ""

    resultCmdlet =
      case node.rec.fmtr of
        BoolFmtr cmdTrue cmdFalse ->
          case node.rec.value of
            BoolValue b ->
              if b then cmdTrue
              else cmdFalse
            _ ->
              "!!! NEITHER TRUE NOR FALSE : " ++ (toString node.rec.value)
              --Debug.crash ("!!! NEITHER TRUE NOR FALSE : " ++ (toString node.rec.value))

        StringFmtr cmdFmt ->
          case node.rec.value of
            StringValue strValue required ->
              if strValue == "" then
                if required then
                    "!! MISSING !!"
                else
                    ""
              else
                sprintf1 cmdFmt strValue
            _ ->
              "!!! NOT A STRING : " ++ (toString node.rec.value)
              --Debug.crash ("!!! NOT A STRING : " ++ (toString node.rec.value))

        KidsListFmtr sFmt listSep ->
          sprintf1 sFmt ( cmdsOfKids listSep node )

{------------------------------------------------------------------------
        KidsByIdFmtr sFmt listSep ->
          -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) SortedById ) sSep )
          -- order of kids is sorted by their Ids
          sprintf1 sFmt ( join listSep ( kidsCmdletsListByIds node ) )
------------------------------------------------------------------------}

        SelectedKidFmtr ->
          case (getSelectedKid (selectedId node.rec) node) of
            Just kid ->
              cmdOf kid
            Nothing ->
              "!!! NOTHING SELECTED : " ++ (toString node.rec.value)

{-------------------------------------------
        EmptyFmtr ->
          --Debug.crash ("!!! EMPTY : " ++ (toString node.fmtr))
          "!!! EMPTY : " ++ (toString node.fmtr)
-------------------------------------------}

  in
    ---Debug.log ("cmdOf " ++ node.id) resultCmdlet
    resultCmdlet


getSelectedKid : Id -> Node -> Maybe Node
getSelectedKid sid node =
  List.head ( List.filter (\ kid -> kid.rec.id == sid ) (kidsOf node) )

sprintf1 : String -> String -> String
sprintf1 str param =
  Regex.replace Regex.All (Regex.regex "({{}}|%s)") (\_ -> param) str

sprintf : String -> String -> String -> String
sprintf str sFmt param =
  Regex.replace Regex.All (Regex.regex sFmt) (\_ -> param) str

insertNodeValue : String -> Node -> String
insertNodeValue str kid =
  sprintf str  ( "{{" ++ kid.rec.id ++ "}}" )  ( cmdOf kid )

cmdListOfKids : Node -> List String
cmdListOfKids node =
  List.map (\ kid -> cmdOf kid) (kidsOf node)

cmdsOfKids : String -> Node -> String
cmdsOfKids listSep node =
  String.join listSep ( cmdListOfKids node )

kidsCmdletsByIdList : Node -> List (Id, String)
kidsCmdletsByIdList node =
  List.map (\ k -> (k.rec.id, cmdOf k)) (kidsOf node)

kidsCmdletsByIdDict : Node -> Dict.Dict Id String
kidsCmdletsByIdDict node =
  Debug.log "kids Cmdlets By Id Dict" ( Dict.fromList ( kidsCmdletsByIdList node ) )

kidsCmdletsListByIds : Node -> List String
kidsCmdletsListByIds node =
  snd ( List.unzip ( Dict.toList ( kidsCmdletsByIdDict node ) ) )


--headList l =
--    case List.head l of
--        Nothing -> []
--        Just h -> [h]


upgrade : Node -> Node -> Node
upgrade defaultRoot oldRoot =
--upgradeHelper : Dict.Dict String Record -> Node -> Node
--upgradeHelper oldNodes_m node =
    let
        oldNodes_m =
            nodeTree2recordByIdDict oldRoot
    in
        upgradeHelper oldNodes_m defaultRoot

nodeTree2recordByIdDict
    : Node
    -> Dict.Dict String Record
--           { descr : String
--           , fmtr : Formatter
--           , label : String
--           , value : Value
--           , id : Id
--           }
nodeTree2recordByIdDict root =
--    (headList nodes)
--    ++ (flatNodes )
    Widget.Data.Flat.flatten root
        |> Array.toList
        |> List.map (\w -> (w.rec.id, w.rec))
        |> Dict.fromList


--upgrade : Dict.Dict (String, Record) -> Node -> Node ->   Node
--upgrade oldNodes_m defaultNode node =
upgradeHelper : Dict.Dict String Record -> Node -> Node
upgradeHelper oldNodes_m node =
--mapUpdate : (Node -> (Node, Cmd a)) -> Node -> (Node, Cmd a)
--mapUpdate f node =
--updateSingleNode : Msg -> Node -> ( Node, Cmd Msg )
--updateSingleNode msg node =
--update : Msg -> Node -> ( Node, Cmd Msg )
--update msg node =
--  mapUpdate (updateSingleNode msg) node
    let
--        oldNodes_m =
--            nodeTree2recordByIdDict node
        newValue =
            case Dict.get node.rec.id oldNodes_m of
                Nothing -> node.rec.value
                Just oldRec -> oldRec.value
        rec = node.rec
        newRecord =
            { rec
            | value = newValue
            }
        kids =
            List.map (upgradeHelper oldNodes_m) <| getKids node
    in
        { node
        | rec = newRecord
        , kids = Kids kids
        }