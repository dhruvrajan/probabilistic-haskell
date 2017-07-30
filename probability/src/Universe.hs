module Universe where

import Node
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as BM

type Universe = Map.Map Int Node

data State = State
  {
    next :: Int,
    universe :: Universe,
    names :: BM.Bimap Int String
  } deriving (Show)


initialState :: Int -> State
initialState i = State {next=i, universe=Map.empty, names=BM.empty}

createNode :: Int -> [Int] -> [Int] -> Payload ->  Node
createNode id parents children payload = Node {identity=id, parents=parents,
                                               children=children, payload=payload}

notifyParents :: Universe -> Int -> [Int] -> Universe
notifyParents universe _ [] = universe
notifyParents universe child (p:ps) = notifyParents universe' child ps where
  parentLookup = Map.lookup p universe
  universe' = case parentLookup of
                Just parent -> Map.insert p newParent universe where
                  cs = children parent
                  newParent = parent {children=child:cs}
                Nothing -> universe
                  
notifyChildren :: Universe -> Int -> [Int] -> Universe
notifyChildren universe _ [] = universe
notifyChildren universe parent (c:cs)
  = notifyChildren universe' parent cs where
  childLookup = Map.lookup c universe
  universe' = case childLookup of
                Just child -> Map.insert c newChild universe where
                  ps = parents child
                  newChild = child {parents=parent:ps}
                Nothing -> universe

notifyUniverse :: Universe -> Node -> Universe
notifyUniverse universe (Node {identity=incoming, parents=ps, children=cs})
  = universe''
  where
    universe' = notifyParents universe incoming ps
    universe'' = notifyChildren universe' incoming cs

add :: State -> String -> Node -> (Node, State)
add state name node = (node', state') where
  n = next state
  node' = node {identity=n}
  newNames = BM.insert n name $ names state
  universe' = notifyUniverse (universe state) node'
  universe'' = Map.insert n node' universe'
  state' = state {next=n + 1, universe=universe'', names=newNames}
