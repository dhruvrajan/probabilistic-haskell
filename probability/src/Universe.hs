module Universe where

import Node
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as BM
import Data.Maybe

type Universe = Map.Map Int Node

data State = State
  {
    next :: Int,
    universe :: Universe,
    names :: Map.Map String Int
  } deriving (Show)


initialState :: Int -> State
initialState i = State {next=i, universe=Map.empty, names=Map.empty}

notify :: (Node -> Node) -> Universe -> Universe
notify = Map.map

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
  newNames = Map.insert name n $ names state
  universe' = notifyUniverse (universe state) node'
  universe'' = Map.insert n node' universe'
  state' = state {next=n + 1, universe=universe'', names=newNames}



addNormal :: State -> String -> Double -> Double ->  (Node, State)
addNormal state name mu sigma = add state name (createDetachedNode $ Normal mu sigma)

addBernoulli :: State -> String -> Double -> (Node, State)
addBernoulli state name p = add state name (createDetachedNode $ Bernoulli p)

addIf :: State -> String -> String -> String -> String -> (Node, State)
addIf state name chk thn els = add state name
  (createNode 0 [fromJust $ Map.lookup chk (names state),
                 fromJust $ Map.lookup thn (names state),
                 fromJust $ Map.lookup els (names state)] [] If)

s = initialState 0
(b, s') = addBernoulli s "b" 0.4
(c, s'') = addBernoulli s' "c" 0.5
(d, s''') = addBernoulli s'' "d" 0.6
(e, s'''') = addIf s''' "e" "b" "c" "d"
