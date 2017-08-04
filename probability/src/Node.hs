{-# LANGUAGE ExistentialQuantification #-}

module Node where

import Prelude hiding (id)
import qualified Data.Map.Strict as Map

data Dist = Bernoulli Double | CPD1 Double Double | CPD2 Double Double Double Double deriving (Show)
data Payload = Observed Dist Bool | Unobserved Dist deriving (Show)


data Node = Node 
  {
    identity :: Int,
    parents :: [Int],
    children :: [Int],
    payload :: Payload
  } deriving (Show)

createNode :: Int -> [Int] -> [Int] -> Payload -> Node
createNode = Node

createDetachedNode :: Payload -> Node
createDetachedNode payload = Node 0 [] [] payload

localProbability :: Node -> [Bool] -> Double
localProbability node vals
  | not $ (length vals) == 1 + (length $ parents node) = 0.0 -- TODO: Proper Error Handling
  | otherwise = case node of
      Node{payload=Unobserved (Bernoulli p)} -> if last vals then p else 1-p
      Node{payload=Unobserved (CPD1 ift iff)} -> if vals !! 0 then
                                                   if last vals then ift else 1-ift
                                                 else
                                                   if last vals then iff else 1 - iff

-- localProbability :: Node -> [Bool] -> Bool -> Double
-- localProbability node ps val
--   | not $ (length ps) == (length $ parents node) = 0.0 -- TODO: Proper Error Handling
--   | otherwise = case node of
--       Node{payload=Unobserved (Bernoulli p)} -> if val then p else 1-p
--       Node{payload=Unobserved (CPD1 ift iff)} -> if ps !! 0 then
--                                                    if val then ift else 1-ift
--                                                  else
--                                                    if val then iff else 1 - iff
