module Util.TreeFlat exposing (..)

import Array
import List

import Html exposing (..)

type alias Record =
  { n    : String
  , kids : Tree
  }

type Tree = Node (List Record)

type alias Wrap =
  { rec     : Record
  , id      : Int
  , parent  : Int
  , parName : String
  }

type alias Model =
  { a  : Array.Array Wrap
  , id : Int
  }

kidsOf : Record -> List Record
kidsOf rec =
  case rec.kids of
    Node kids_l -> kids_l

getRecName : Int -> Array.Array Wrap -> String
getRecName id a =
  case (Array.get id a) of
    Nothing -> ""
    Just wrap -> wrap.rec.n

getWrap : Int -> Array.Array Wrap -> Wrap
getWrap id a =
  case (Array.get id a) of
    Nothing -> notFoundWrap <| "idx " ++ (toString id) ++ " not found"
    Just wrap -> wrap

flatten : Record -> Array.Array Wrap
flatten record =
  let
    model = Model Array.empty 100
    fmodel = flattenHelp model [record] -100
  in
    fmodel.a

flattenHelp : Model -> List Record -> Int -> Model
flattenHelp model records_l parId =
  let
    restRecords =
      case List.tail records_l of
        Nothing ->          []
        Just rest_l ->      rest_l
  in
    case List.head records_l of
      Nothing ->
        model
      Just record ->
        let
          rec = { record | kids = Node [] }
          id = Array.length model.a
          wrap = Wrap rec id parId (getRecName parId model.a)
          nModel = { model
                   | a = Array.push wrap model.a
                   , id = id
                   }
          kidsModel = flattenHelp nModel (kidsOf record) id
        in
          flattenHelp kidsModel restRecords parId

deflatten : Array.Array Wrap -> Record
deflatten a =
  let
    model = Model a -200
    nModel = Array.foldr deflattenHelper model a
  in
    case Array.get 0 nModel.a of
      Nothing ->
        notFoundRec "Idx 0 in Array not found"
      Just wrap ->
        wrap.rec

notFoundRec : String -> Record
notFoundRec errMsg =
  Record errMsg (Node [])

notFoundWrap : String -> Wrap
notFoundWrap errMsg =
  Wrap (notFoundRec errMsg) -1111 -11111 "NOT FOUND"

deflattenHelper : Wrap -> Model -> Model
deflattenHelper loopWrap model =
  let
    parId = wrap.parent
    parWrap =
      getWrap parId model.a
    parRec =
      parWrap.rec
    wrap =
      getWrap loopWrap.id model.a
    newParRec = { parRec | kids = Node (wrap.rec :: (kidsOf parRec) ) }
    newParWrap = { parWrap | rec = newParRec }
    nParWrap = Debug.log ("deflatten: new par wrap " ++ (toString parId)) newParWrap
  in
    { model | a = Array.set parId nParWrap model.a }


testRecsAsHtml : List Record -> Html a
testRecsAsHtml recs_l =
  ulOf flatten toString deflatten recs_l

main : Html a
main =
  testRecsAsHtml [t1, t2, t5]



{---------------------------------------------------------
---------------------------------------------------------}

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

{---------------------------------------------------------
---------------------------------------------------------}
