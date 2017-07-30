{-# LANGUAGE ExistentialQuantification #-}
module Node where

import Prelude hiding (id)
import qualified Data.Map.Strict as Map
import Distribution

data Payload = Payload deriving (Show)


data Node = Node
  {
    identity :: Int,
    parents :: [Int],
    children :: [Int],
    payload :: Payload    
  } deriving (Show)
