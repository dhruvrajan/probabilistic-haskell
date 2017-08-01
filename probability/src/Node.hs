{-# LANGUAGE ExistentialQuantification #-}

module Node where

import Prelude hiding (id)
import qualified  Distribution as Dist
import qualified Data.Map.Strict as Map
import qualified Distribution as Dist

data Payload = Normal Double Double | Bernoulli Double | If deriving (Show)


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


