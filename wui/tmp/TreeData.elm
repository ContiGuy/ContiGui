module Util.TreeData exposing (..)

import Array
import List

import Html exposing (..)

type alias Record =
  { n    : String
  }

type alias Node =
  { rec  : Record
  , kids : Tree
  }

type Tree = Kids (List Node)

type alias Wrap =
  { rec     : Record
  , id      : Int
  , parent  : Int
  }

type alias FModel =
  { wa    : Array.Array Wrap
  , id   : Int
  }

type alias DModel =
  { na    : Array.Array Node
  }

notFoundRec : String -> Record
notFoundRec errMsg =
  Record errMsg

notFoundWrap : String -> Wrap
notFoundWrap errMsg =
  Wrap (notFoundRec errMsg) -1111 -11111

notFoundNode : String -> Node
notFoundNode errMsg =
  Node (notFoundRec errMsg) (Kids [])

kidsOf : Node -> List Node
kidsOf node =
  case node.kids of
    Kids kids_l -> kids_l

insertKid : Node -> Node -> Node
insertKid newKid node =
  { node | kids = Kids ( newKid :: (kidsOf node) ) }


flatten : Node -> Array.Array Wrap
flatten node =
  let
    model = FModel Array.empty 100
    fmodel = flattenHelp model [node] -100
  in
    fmodel.wa

flattenHelp : FModel -> List Node -> Int -> FModel
flattenHelp model nodes_l parId =
  let
    restNodes =
      case List.tail nodes_l of
        Nothing ->          []
        Just rest_l ->      rest_l
  in
    case List.head nodes_l of
      Nothing ->
        model
      Just node ->
        let
          nod = { node | kids = Kids [] }
          id = Array.length model.wa
          wrap = Wrap nod.rec id parId
          nModel = { model
                   | wa = Array.push wrap model.wa
                   , id = id
                   }
          kidsModel = flattenHelp nModel (kidsOf node) id
        in
          flattenHelp kidsModel restNodes parId

deflatten : Array.Array Wrap -> Node
deflatten wa =
  let
    modelEmpty =
      DModel Array.empty
    modelWithRecords =
      Array.foldl deflattenRecordsHelper modelEmpty wa
    modelWithNodes =
      Array.foldr deflattenNodesHelper modelWithRecords wa
  in
    case Array.get 0 modelWithNodes.na of
      Nothing ->
        let
          _ = Debug.log "deflatten: na" <| toString modelWithNodes.na
        in
          notFoundNode "Idx 0 in Array not found"
      Just node ->
        node

deflattenRecordsHelper : Wrap -> DModel -> DModel
deflattenRecordsHelper wrap model =
  let
    msgName =
      "deflattenRecordsHelper set @ " ++ (toString <| Array.length model.na)
    loggedWrap = Debug.log msgName wrap
  in
    { model | na = Array.push (Node loggedWrap.rec (Kids [])) model.na }


getNodeWithDefault : Int -> Array.Array Node -> Record -> Node
getNodeWithDefault id nodeArray defaultRecord =
  Maybe.withDefault
    ( Node defaultRecord (Kids []) )
    ( Array.get id nodeArray )


deflattenNodesHelper : Wrap -> DModel -> DModel
deflattenNodesHelper loopWrap model =
  let
    node =
      getNodeWithDefault loopWrap.id model.na
        loopWrap.rec

    logMsg =
      "deflatten: set new node @ " ++ (toString loopWrap.id)
    loggedNode =
      Debug.log logMsg node

    modelWithNode =
      { model
      | na = Array.set loopWrap.id loggedNode model.na
      }
  in
    if loopWrap.parent < 0 then
      modelWithNode
    else
      let
        parNode =
          getNodeWithDefault loopWrap.parent model.na
            <| notFoundRec "Not Yet Unwrapped"
        newParNode =
          insertKid node parNode
        logMsg =
          "deflatten: set new parent node @ " ++ (toString loopWrap.parent)
        loggedParNode =
          Debug.log logMsg newParNode
      in
        { modelWithNode
        | na = Array.set loopWrap.parent loggedParNode model.na
        }


{---------------------------------------------------------
---------------------------------------------------------}

main : Html a
main =
  testRecsAsHtml [t1, t2, t5]
--  testRecsAsHtml [t4]


testRecsAsHtml : List Node -> Html a
testRecsAsHtml nodes_l =
  ulOf flatten toString deflatten nodes_l


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

sampleNode : id -> List Node -> Node
sampleNode id kids_l =
  Node (Record ("r" ++ (toString id))) (Kids kids_l)

t1 : Node
t1 = sampleNode 1 [t2, t3]
t2 : Node
t2 = sampleNode 2 [t4, t5]
t3 : Node
t3 = sampleNode 3 []
t4 : Node
t4 = sampleNode 4 []
t5 : Node
t5 = sampleNode 5 [t6, t7]
t6 : Node
t6 = sampleNode 6 []
t7 : Node
t7 = sampleNode 7 []

{---------------------------------------------------------
---------------------------------------------------------}
