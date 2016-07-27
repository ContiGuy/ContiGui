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

module ComboBox exposing (..)

import Html -- exposing (..)
import Html.Events -- exposing (..)
import Html.Attributes -- exposing (..)
-- import Html.App
import Css -- exposing (..)

import String
--import Dict

{----------------------------------------------
main =
  Html.App.program {
    -- init = ( init "Choose" Success, Cmd.none ),
    init = ( init "Choose", Cmd.none ),
    view = view identity Action,
    update = update,
    subscriptions = \_ -> Sub.none
  }
----------------------------------------------}


-- MODEL

type alias Model =
    { current    : String
    , mutable    : Bool
--    , editField  : String
    --, entries    : Dict.Dict String String
    , entries    : List String
    , debug      : Bool
    }

{------------------------------
------------------------------}

init : List String -> Model
init entries =
  --Model "" "default" (Dict.fromList [("","")]) False
  --Model "" "default" ["", "aa", "bb"] False
--  Model "" "" entries False
  Model "" False entries False

testInit : Model
testInit =
  --Model "" "default" (Dict.fromList [("","")]) False
  --Model "" "default" ["", "aa", "bb"] False
  init ["aa", "bbbb"]



{------------------------------
------------------------------}


-- UPDATE

type Msg
  = UpdateField String
  | NoOp
  | Select String
--  | Success String
  | ToggleDebug Bool
  --| NewOptions ( List (String, String) )
  | NewOptions ( List String )


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
        model ! []

    UpdateField str ->
      { model
--      | editField = str
      | current = str
--      , current = ""
      } ! []

    Select str ->
      --updateWith False str model
      { model
      | current = str
--      , editField = str
      } ! []

--    Success str ->
--      updateWith True str model

    ToggleDebug dbg ->
      { model | debug = dbg } ! []

    -- NewOptions ( List String )
    NewOptions newOpts ->
      let
--        ( newField, newEntries ) =
        model' =
          case ( List.head newOpts ) of
            Just firstEntry ->
--              ( firstEntry, newOpts )

                { model
                --| entries = Dict.fromList newEntries
                | entries = listTail newOpts
                , current = firstEntry
--                , editField = firstEntry
                }

            Nothing ->
--              ( "", [] )
              model
      in
        { model'
        | mutable = not <| List.isEmpty newOpts
        } ! []
--        { model
--        --| entries = Dict.fromList newEntries
--        | entries = newEntries
--        , current = newField
--        , editField = ""
--        } ! []


listTail : List a -> List a
listTail l =
    case List.tail l of
        Nothing  -> []
        Just t_l -> t_l

--updateWith : Bool -> String -> Model -> ( Model, Cmd Msg )
--updateWith updateEntries str model =
--      let
--        m =
--          if updateEntries then "Success" else "Select"
--        s =
--          if model.debug then Debug.log ( "CB." ++ m ) str
--          else str
--        nEntries =
--          if updateEntries then
--            str :: List.filter (\ e -> e /= s ) model.entries
--          else
--            model.entries
--      in
--        { model
--        | current = str
--        , editField = ""
--        , entries = nEntries
--        } ! []


-- VIEW

viewTest : Model -> Html.Html Msg
viewTest model =
  Html.table [] [ Html.tr [] [
    --Html.td [] [ viewX [ Html.text "Test: Pick new" ] "--" Select model ]  --.combo ]
    Html.td [] [ viewX [ Html.text "Test: Pick new" ] "--" Select model ]  --.combo ]
--  , Html.td [] [ viewButton (\ s -> Html.text ( "Save '" ++ s ++ "' !" ) ) Success model]  --.combo ]
--  , Html.td [] [ viewButton (\ s -> Html.text "Save" ) Success model]  --.combo ]
  --, Html.td [] [ Html.label [] [ Html.text "Test: Pick new" ] ]
--  , Html.td [] [ --Html.App.map ComboMsg
  --                  ( viewField model ) ]  --.combo ) ]
--  , Html.td [] [ --Html.App.map ComboMsg
  --                  ( viewDbg model ) ]  -- .combo ) ]
  ] ]



--view : Model -> Html.Html msg
--view model =
view : List String -> String -> (String -> Msg) -> Model -> Html.Html Msg
view labels neutralEntry selectMsg model =
    let
        xLabels = List.map Html.text labels
    in
        viewX xLabels neutralEntry selectMsg model

viewX : List (Html.Html Msg) -> String -> (String -> Msg) -> Model -> Html.Html Msg
viewX labels neutralEntry selectMsg model =
    let
        txt s =
          if s == "" then
            neutralEntry
          else
            s
--        txt1 s =
--          txt (
--              if s == model.current then
--                "[ " ++ s ++ " ]"
--              else
--                s
--              )

--        selection0 s =
--            [ Html.Events.onClick (selectMsg s) ] ++ [
--            {--------------------------------------}
--                if s == model.current then
--                  Html.Attributes.style [ ("backgoundColor", "blue") ]
--                else
--                  Html.Attributes.style []
--            --------------------------------------}
--            ]

        selectMsg' s =
            if s == model.current then NoOp
--            if s == model.editField then NoOp
            else selectMsg s

        selectionStyle s =
            [ Html.Events.onClick (selectMsg' s) ] ++
            [
            {--------------------------------------}
                if s == model.current then
                  --Html.Attributes.style [ ("backgound-color", "blue") ]
                  stylesheet.class DropDownSelected
                else
                  --Html.Attributes.style []
                  --[ stylesheet.class DropDownContent ]
                  stylesheet.class DropDownUnselected
            --------------------------------------}
            ]
    in
      Html.div []
      [ Css.style [] stylesheet
      , Html.div [ stylesheet.class DropDown ]
        [
          Html.table [] [ Html.tr [] [
            Html.td [] labels
          , Html.td []
              [ viewField model
              , Html.table [ stylesheet.class DropDownContent ]
                  ( List.map (\ lbl ->
                    Html.tr [ Html.Attributes.style [ ("width", "100%") ] ]
                    [ Html.td [ Html.Attributes.style [ ("width", "100%") ] ]
                      [ Html.button
                        --( selection0 lbl )
--                        [ Html.Attributes.style
--                          [ ("width", "100%")
--                          , ("background-color", "#bbbbbb")
--                          ]
--                        ]
                        ( Html.Attributes.style [ ("width", "100%") ] :: ( selectionStyle lbl ))
                        [ --Html.label (selectionStyle lbl) [
                          Html.text (txt lbl)
                        ] --]
                      ]
                      {----------------------------
                      [ Html.button ( selection0 lbl )
                        [ --Html.label (selectionStyle lbl) [
                          Html.text (txt lbl)
                        ] --]
                      ]
                      --------------------------------}
                    ]
                  ) model.entries )
              ]
          , Html.td [] [ viewDbg model ]
          ] ]
        ]
      ]

--{-------------------------------------------------------------------}
--viewOption0 : String -> (String -> msg) -> Model -> Html.Html msg
--viewOption0 neutralEntry selectMsg model =
--  let
--    txt s =
--      if s == "" then
--        neutralEntry
--      else
--        s
--    optAttrs s =
--      ( Html.Attributes.selected ( s == model.current ) ) :: (
--        if s == model.current then
--          []
--        else
--          [ Html.Events.onClick (selectMsg s) ]
--      )
--    opt s =
--      Html.option ( optAttrs s ) [ Html.text (txt s) ]
--  in
--    Html.select []
--        ( List.map opt model.entries )
---------------------------------------------------------------------}




{------------------------------------------------------------------
<style>
.dropdown {
    position: relative;
    display: inline-block;
}

.dropdown-content {
    display: none;
    position: absolute;
    background-color: #f9f9f9;
    min-width: 160px;
    box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
    padding: 12px 16px;
    z-index: 1;
}

.dropdown:hover .dropdown-content {
    display: block;
}
</style>

<div class="dropdown">
  <span>Mouse over me</span>
  <div class="dropdown-content">
    <p>Hello World!</p>
  </div>
</div>
------------------------------------------------------------------}

--isEmptyString : String -> Bool
--isEmptyString s =
--  String.isEmpty ( String.trim s )

--viewButton : (String -> Html.Html msg) -> (String -> msg) -> Model -> Html.Html msg
--viewButton labeller actionMsg model =
--  let
--      currentIsEmpty   = isEmptyString model.current
--      editFieldIsEmpty = isEmptyString model.editField
--
--{-------------------------------------------------------------------
---------------------------------------------------------------------}
--      isBlocked =
--        currentIsEmpty && editFieldIsEmpty
--      actionStr =
--        if currentIsEmpty then
--          String.trim model.editField
--        else
--          String.trim model.current
--  in
--      Html.button [ Html.Events.onClick ( actionMsg actionStr )
--             , Html.Attributes.disabled isBlocked
--             ] [ labeller actionStr ]


viewField : Model -> Html.Html Msg
viewField model =
  let
    currentIsEmpty = String.isEmpty ( String.trim model.current )
  in
{-------------------------------------------------------------------
-------------------------------------------------------------------}
    Html.input [
      Html.Attributes.type' "text"
--    , Html.Attributes.value model.editField
    , Html.Attributes.value model.current
    , Html.Events.onInput UpdateField
    --, disabled ( not currentIsEmpty )
    , Html.Attributes.disabled <| not model.mutable
    , Html.Attributes.autofocus True

    --, placeholder "What needs to be done?"
    --, name "newTodo"
    --, onInput UpdateField
    --, onEnter Add
    ] []



{--------------------------------------------------------------}
viewDbg : Model -> Html.Html Msg
viewDbg model =
  let
    dbgInfo =
      if model.debug then
        Html.text ( toString model )
      else
        Html.div [] []
  in
      Html.div [] [
        Html.label [] [ Html.text "debug" ]
      , Html.input [
          Html.Attributes.type' "checkbox"
          , Html.Attributes.checked model.debug
          , Html.Events.onCheck ToggleDebug
        ] []
      , dbgInfo
      ]

{--------------------------------------------------------------
------------------------------------------------------------}


type Class = DropDown | DropDownContent | DropDownSelected | DropDownUnselected

-- import a font
imports : List a
--imports = ["https://fonts.googleapis.com/css?family=Droid+Sans:400"]
imports = []

-- create a rule
dropDownRule :
    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
dropDownRule =
    { selectors = [Css.Class DropDown]
    , descriptor = [
        ("position", "relative")
      , ("display", "inline-block")
      ]
    }

-- create a rule
dropDownContentRule :
    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
dropDownContentRule =
    { selectors = [Css.Class DropDownContent]
    , descriptor = [
        ("position", "absolute")
      , ("display", "none")
      {------------------------------------------------------------ }
        background-color: #f9f9f9;
        min-width: 60px;
        max-width: 160px;
        box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
        padding: 12px 16px;
        height: 150px;
        overflow: scroll;
      ------------------------------------------------------------}
      ]
    }

-- create a rule
dropDownHoverRule :
    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
dropDownHoverRule =
    { selectors = [Css.Descendant (Css.Class DropDownContent) (Css.Pseudo [Css.Hover] (Css.Class DropDown)) ]
    , descriptor = [
        ("display", "block")
      --, ("backgound-color", "green")
      ]
    }

-- create a rule
dropDownUnselectedRule :
    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
dropDownUnselectedRule =
    { selectors = [Css.Class DropDownUnselected]
    , descriptor = [
--        ("background-color", "#f0f0f0")
      --, ("display", "inline-block")
      ]
    }

-- create a rule
dropDownSelectedRule :
    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
dropDownSelectedRule =
    { selectors = [Css.Class DropDownSelected]
    , descriptor = [
--        ("background-color", "#404040")
      --, ("display", "inline-block")
      ]
    }

-- create the stylesheet
stylesheet : Css.Stylesheet a Class b
stylesheet = Css.stylesheet imports
    [ dropDownHoverRule
    , dropDownRule
    , dropDownContentRule
    , dropDownSelectedRule
    , dropDownUnselectedRule
    ]

