module Node where

import Prelude hiding (id)
import qualified Data.Map.Strict as Map

data Node = Node {
                   id :: Int,
                   parents :: [Int],
                   children :: [Int]
                 }

type Universe = Map.Map Int Node

data State = State {
                     next :: Int,
                     universe :: Universe
                   }


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
notifyChildren universe parent (c:cs) = notifyChildren universe' parent cs where
  childLookup = Map.lookup c universe
  universe' = case childLookup of
                Just child -> Map.insert c newChild universe where
                  ps = parents child
                  newChild = child {parents=parent:ps}
                Nothing -> universe

notifyUniverse :: Universe -> Node -> Universe
notifyUniverse universe (Node {id=incoming,
                               parents=ps,
                               children=cs}) = universe''
  where
    universe' = notifyParents universe incoming ps
    universe'' = notifyChildren universe' incoming cs

add :: State -> Node -> (Node, State)
add state node = (node', state') where
  n = next state
  node' = node {id=n}
  universe' = notifyUniverse (universe state) node'
  state' = state {next=n + 1, universe=universe'}
  
