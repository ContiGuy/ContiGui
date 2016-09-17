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

module ComboBox exposing (..)

import Html -- exposing (..)
import Html.Events -- exposing (..)
import Html.Attributes -- exposing (..)
-- import Html.App
import Css -- exposing (..)

import String

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
    , entries    : List String
    , debug      : Bool
    , errMsg     : String
    }

{------------------------------
------------------------------}

init : List String -> Model
init entries =
  Model "" False entries False ""

testInit : Model
testInit =
  init ["aa", "bbbb"]



{------------------------------
------------------------------}


-- UPDATE

type Msg
  = UpdateField String
  | FieldChanged String
  | Select String
  | ToggleDebug Bool
  | NewOptions ( List String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
--    NoOp ->
--        model ! []

    UpdateField str ->
        let
            eMsg =
                if List.member str model.entries then
                    "DUCPLICATE"
                else
                    ""
        in
          { model
          | current = str
          , errMsg = eMsg
          } ! []

    FieldChanged str ->
      { model
      | current = str
      } ! []

    Select str ->
      { model
      | current = str
      } ! []

    ToggleDebug dbg ->
      { model | debug = dbg } ! []

    -- NewOptions ( List String )
    NewOptions newOpts ->
      let
        model' =
          case ( List.head newOpts ) of
            Just firstEntry ->
                { model
                | entries = listTail newOpts
                , current = firstEntry
                }
            Nothing ->
              model
      in
        { model'
        | mutable = not <| List.isEmpty newOpts
        } ! []


listTail : List a -> List a
listTail l =
    case List.tail l of
        Nothing  -> []
        Just t_l -> t_l



-- VIEW

viewTest : Model -> Html.Html Msg
viewTest model =
  Html.table []
  [ Html.tr []
    [ Html.td [] [ viewX [ Html.text "Test: Pick new" ] "--" Select model ]
    ]
  ]



view : List String -> String -> (String -> Msg) -> Model -> Html.Html Msg
view labels neutralEntry selectMsg model =
    let
        xLabels = List.map Html.text labels
    in
        viewX xLabels neutralEntry selectMsg model

viewX : List (Html.Html Msg) -> String -> (String -> Msg) -> Model -> Html.Html Msg
viewX labels neutralEntry selectMsg model =
--    let
--        txt s =
--          if s == "" then
--            neutralEntry
--          else
--            s

--        selection0 s =
--            [ Html.Events.onClick (selectMsg s) ] ++ [
--            {--------------------------------------}
--                if s == model.current then
--                  Html.Attributes.style [ ("backgoundColor", "blue") ]
--                else
--                  Html.Attributes.style []
--            --------------------------------------}
--            ]

--        selectMsg' s =
--            if s == model.current then NoOp
--            else selectMsg s
--
--        selectionStyle s =
--            [ Html.Events.onClick (selectMsg' s) ]
--    in
      Html.div []
      [ Css.style [] stylesheet
      , Html.div [ stylesheet.class DropDown ]
        [ Html.table seamlessStyle
          [ Html.tr seamlessStyle
            [ Html.td seamlessStyle labels
            , Html.td seamlessStyle
              [ viewField model
              , Html.table [ stylesheet.class DropDownContent ]
                ( List.map (\ lbl ->
                    Html.tr []
                    [ Html.td seamlessStyle
                      [ Html.button
                        [ Html.Events.onClick (selectMsg lbl)
                        , seamlessStyleWithAttrs
                          [ ("width", "100%")
                          , ("background-color", "#E7FAC4")
                          ]
                        ]
                        [ Html.text lbl   -- (txt lbl)
                        ]
                      ]
                    ]
                  ) model.entries
                )
              ]
            , Html.td [] [ viewDbg model ]
            ]
          ]
        ]
      ]

seamlessStyleWithAttrs : List ( String, String ) -> Html.Attribute a
seamlessStyleWithAttrs attrs =
    Html.Attributes.style
        ( seamlessAttrs ++ attrs )

seamlessStyle : List (Html.Attribute a)
seamlessStyle =
    [ Html.Attributes.style seamlessAttrs ]

seamlessAttrs : List ( String, String )
seamlessAttrs =
      [ ("border", "none")
      , ("padding", "0")
      ]


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
    (styles, err) =
        if model.errMsg == "" then
            ([], [])
        else
            ( [ ("color", "red") ]
            , [ Html.label [ Html.Attributes.style [("color", "red")] ]
                [ Html.text model.errMsg ]
              ]
            )
    style = Html.Attributes.style styles
  in
{-------------------------------------------------------------------
-------------------------------------------------------------------}
    Html.div []
    ( [ Html.input
        [ Html.Attributes.type' "text"
        , Html.Attributes.value model.current
        , Html.Events.onInput UpdateField
        --, disabled ( not currentIsEmpty )
        , Html.Attributes.disabled <| not model.mutable
        , Html.Attributes.autofocus True
        , style

        --, placeholder "What needs to be done?"
        --, name "newTodo"
        --, onInput UpdateField
        --, onEnter Add

        , Html.Events.onBlur <| FieldChanged model.current
        ]
        []
      ] ++ err
    )



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
      Html.div []
      [ Html.label [] [ Html.text "debug" ]
      , Html.input [
          Html.Attributes.type' "checkbox"
          , Html.Attributes.checked model.debug
          , Html.Events.onCheck ToggleDebug
        ] []
      , dbgInfo
      ]

{--------------------------------------------------------------
------------------------------------------------------------}


type Class
    = DropDown
    | DropDownContent
--    | DropDownSelected
--    | DropDownUnselected

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
      ,
        ("display", "none")
--      , ("backgound-color", "#ffb366")
      , ("background-color", "#E7FAC4")
      , ("width", "100%")
--      , ("overflow", "scroll")
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
    , descriptor =
      [ ("display", "block")
      , ("background-color", "#E7FAC4")
--      , ("backgound-color", "lightblue")
--      , ("backgound-color", "#ffb366")
--      , ("width", "100%")
      ]
    }

---- create a rule
--dropDownUnselectedRule :
--    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
--dropDownUnselectedRule =
--    { selectors = [Css.Class DropDownUnselected]
--    , descriptor = [
----        ("background-color", "#f0f0f0")
--        ("backgound-color", "#66ffd9")
--      --, ("display", "inline-block")
--      , ("width", "100%")
--      ]
--    }
--
---- create a rule
--dropDownSelectedRule :
--    { descriptor : List ( String, String ), selectors : List (Css.Sel a Class) }
--dropDownSelectedRule =
--    { selectors = [Css.Class DropDownSelected]
--    , descriptor = [
----        ("background-color", "#404040")
--        ("backgound-color", "#6666ff")
--      --, ("display", "inline-block")
--      , ("width", "100%")
--      ]
--    }

-- create the stylesheet
stylesheet : Css.Stylesheet a Class b
stylesheet = Css.stylesheet imports
    [ dropDownHoverRule
    , dropDownRule
    , dropDownContentRule
--    , dropDownSelectedRule
--    , dropDownUnselectedRule
    ]

