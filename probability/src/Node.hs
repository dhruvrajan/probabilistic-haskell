{-# LANGUAGE ExistentialQuantification #-}

module Node where

import Prelude hiding (id)
import qualified  Distribution as Dist
import qualified Data.Map.Strict as Map
import qualified Distribution as Dist

data Payload = Bernoulli Double | If deriving (Show)
--data Payload a = Normal a a | Bernoulli Double a a deriving (Show)


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


