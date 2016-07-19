module Util.TreeData exposing (..)

import Util.CoreData exposing (..)
import Util.CoreJson exposing (..)

import Array
import List

import Html exposing (..)


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


unwrapNode : Wrap -> Node
unwrapNode wrap =
  Node wrap.rec (Kids [])

deflatten : Array.Array Wrap -> Node
deflatten wa =
  let
    modelWithRecords = DModel
      <| Array.map unwrapNode wa
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


encWrap : Wrap -> Json.Encode.Value
encWrap wrap =
  Json.Encode.object [
    ( "rec",     encRec wrap.rec )
  , ( "id",      Json.Encode.int wrap.id )
  , ( "par-id",  Json.Encode.int wrap.parent )
  --, ( "par-n",   Json.Encode.string wrap.parName )
  ]

decWrap : Json.Decode.Decoder Wrap
decWrap =
  Json.Decode.object3 Wrap
    ( "rec" := decRec )
    ( "id" := Json.Decode.int )
    ( "par-id" := Json.Decode.int )
    --( "par-n" := Json.Decode.string )

encWrapArray : Array.Array Util.TreeFlat.Wrap -> Json.Encode.Value
encWrapArray wraps_l =
  Json.Encode.array (Array.map encWrap wraps_l)

decWrapArray : Json.Decode.Decoder (Array.Array Wrap)
decWrapArray =
  Json.Decode.array decWrap

wraps2json : Array.Array Wrap -> String
wraps2json wraps_a =
  Json.Encode.encode 2 <| encWrapArray wraps_a

json2wraps : String -> Result String (Array.Array Wrap)
json2wraps json_s =
  Json.Decode.decodeString decWrapArray json_s


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
