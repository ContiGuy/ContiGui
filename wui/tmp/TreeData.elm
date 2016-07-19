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
  --, parName : String
  }

type alias FModel =
  { wa    : Array.Array Wrap
  , id   : Int
  }

type alias DModel =
  { na    : Array.Array Node
  --, id   : Int
  --, tree : Node
  }

notFoundRec : String -> Record
notFoundRec errMsg =
  Record errMsg   -- (Node [])

notFoundWrap : String -> Wrap
notFoundWrap errMsg =
  Wrap (notFoundRec errMsg) -1111 -11111    -- "NOT FOUND"

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

{----------------------------------------------
getRecName : Int -> Array.Array Wrap -> String
getRecName id a =
  case (Array.get id a) of
    Nothing -> ""
    Just wrap -> wrap.rec.n
----------------------------------------------}

getWrap : Int -> Array.Array Wrap -> Wrap
getWrap id wa =
  case (Array.get id wa) of
    Nothing -> notFoundWrap <| "idx " ++ (toString id) ++ " not found"
    Just wrap -> wrap

getNodeWithDefault : Int -> Array.Array Node -> Node -> Node
getNodeWithDefault id na defNode =
  case (Array.get id na) of
    Nothing -> defNode  -- notFoundNode <| "idx " ++ (toString id) ++ " not found"
    Just node -> node

flatten : Node -> Array.Array Wrap
flatten node =
  let
    --model = Model Array.empty 100 <| notFoundNode "NOT NEEDED for FLATTENing"
    model = FModel Array.empty 100    -- <| notFoundNode "NOT NEEDED for FLATTENing"
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
          wrap = Wrap nod.rec id parId     -- (getRecName parId model.a)
          nModel = { model
                   | wa = Array.push wrap model.wa
                   , id = id
                   }
          kidsModel = flattenHelp nModel (kidsOf node) id
        in
          flattenHelp kidsModel restNodes parId

deflatten : Array.Array Wrap -> Node
deflatten a =
  let
    --model = Model a -200 <| notFoundNode "NOT STARTED"
    model = DModel Array.empty
    nModel = Array.foldr deflattenHelper model a
  in
    --nModel.tree
    {--------------------------------------------}
    case Array.get 0 nModel.na of
      Nothing ->
        notFoundNode "Idx 0 in Array not found"
      Just node ->
        node
    --------------------------------------------}

deflattenHelper : Wrap -> DModel -> DModel
deflattenHelper loopWrap model =
  let
    parId = loopWrap.parent
    parNode =
      getNodeWithDefault parId model.na
        <| Node (notFoundRec "Not Yet Unwrapped") (Kids [])
      {----------------------------
      if model.id == parId then
        model.tree
      else
        Node loopWrap.rec (Kids [])
      ----------------------------}
    id = loopWrap.id
    node =
      getNodeWithDefault id model.na
        <| Node (notFoundRec "NOT FOUND") (Kids [])
    newParNode =
      insertKid node parNode

    --parWrap =
      --getWrap parId model.a
    --parRec =
      --parWrap.rec
    --wrap =
      --getWrap loopWrap.id model.a
    --newParRec = { parRec | kids = Node (wrap.rec :: (kidsOf parRec) ) }
    --newParWrap = { parWrap | rec = newParRec }
    -- nParWrap = 7  -- Debug.log ("deflatten: new par wrap " ++ (toString parId)) newParWrap
  in
    -- { model | a = Array.set parId nParWrap model.a }
    { model | na = Array.set parId newParNode model.na }


testRecsAsHtml : List Node -> Html a
testRecsAsHtml nodes_l =
  ulOf flatten toString deflatten nodes_l

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

sampleNode : id -> List Node -> Node
sampleNode id kids_l =
  Node (Record ("r" ++ (toString id))) (Kids kids_l)

t1 : Node
--t1 = Node (Record "r1") (Kids [t2, t3])
t1 = sampleNode 1 [t2, t3]
t2 : Node
--t2 = Node "r2" (Kids [t4, t5])
t2 = sampleNode 2 [t4, t5]
t3 : Node
t3 = sampleNode 3 []
--t3 = Node "r3" (Kids [])
t4 : Node
t4 = sampleNode 4 []
--t4 = Node "r4" (Kids [])
t5 : Node
t5 = sampleNode 5 [t6, t7]
--t5 = Node "r5" (Kids [t6, t7])
t6 : Node
t6 = sampleNode 6 []
--t6 = Node "r6" (Kids [])
t7 : Node
t7 = sampleNode 7 []
--t7 = Node "r7" (Kids [])

{---------------------------------------------------------
---------------------------------------------------------}
