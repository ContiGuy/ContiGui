module Widget.Data.Flat exposing (..)

import Widget.Data.Type exposing (..)

import Array
import List


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
--        let
--          _ = Debug.log "deflatten: na" <| toString modelWithNodes.na
--        in
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

--    logMsg =
--      "deflatten: set new node @ " ++ (toString loopWrap.id)
--    loggedNode =
--      Debug.log logMsg node

    modelWithNode =
      { model
--      | na = Array.set loopWrap.id loggedNode model.na
      | na = Array.set loopWrap.id node model.na
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
--        logMsg =
--          "deflatten: set new parent node @ " ++ (toString loopWrap.parent)
--        loggedParNode =
--          Debug.log logMsg newParNode
      in
        { modelWithNode
--        | na = Array.set loopWrap.parent loggedParNode model.na
        | na = Array.set loopWrap.parent newParNode model.na
        }


{---------------------------------------------------------
---------------------------------------------------------}
