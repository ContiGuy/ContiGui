module TreeFlat exposing (..)

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
          id = (Array.length model.a) + 1
          rec = { record | kids = Node [] }
          wrap = Wrap rec parId (getRecName (parId-1) model.a)
          nModel = { model
                   | a = Array.push wrap model.a
                   , id = id
                   }
          kidsModel = flattenHelp nModel (kidsOf record) id
        in
          flattenHelp kidsModel restRecords parId

getRecName : Int -> Array.Array Wrap -> String
getRecName id a =
  case (Array.get id a) of
    Nothing -> ""
    Just wrap -> wrap.rec.n

ul1Of tree =
  ul [] [
    li [] [ text <| toString tree ]
  , ul [] [ li [] [ text <| toString <| flatten tree ] ]
  ]

ulOf treeList =
  ul [] ( List.concat (List.map liPair treeList) )

liPair tree =
  [ li [] [ text <| toString tree ]
  , ul [] [ li [] [ text <| toString <| flatten tree ] ]
  ]

t1 = Record "r1" (Node [t2, t3])
t2 = Record "r2" (Node [t4, t5])
t3 = Record "r3" (Node [])
t4 = Record "r4" (Node [])
t5 = Record "r5" (Node [t6, t7])
t6 = Record "r6" (Node [])
t7 = Record "r7" (Node [])

main =
  ulOf [t1, t2, t5]
