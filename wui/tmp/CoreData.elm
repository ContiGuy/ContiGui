module Util.CoreData exposing (..)

import Array
import List

import Html exposing (..)

{---------------------------------------}
type alias Record =
  { n    : String
  }

type alias Node =
  { rec  : Record
  , kids : Tree
  }

type Tree = Kids (List Node)

kidsOf : Node -> List Node
kidsOf node =
  case node.kids of
    Kids kids_l -> kids_l

insertKid : Node -> Node -> Node
insertKid newKid node =
  { node | kids = Kids ( newKid :: (kidsOf node) ) }
---------------------------------------}

{---------------------------------------
---------------------------------------}

notFoundRec : String -> Record
notFoundRec errMsg =
  Record errMsg

notFoundNode : String -> Node
notFoundNode errMsg =
  Node (notFoundRec errMsg) (Kids [])
---------------------------------------}

