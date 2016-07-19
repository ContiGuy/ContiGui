module Widget.Gen exposing (..)

import Widget.Data.Type exposing (..)

import List
import String -- exposing (contains)
import Regex  -- as RX
import Dict



cmdOf : Node -> String
cmdOf node =
  let
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
            StringValue strValue ->
              if strValue == "" then
                --strValue
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
