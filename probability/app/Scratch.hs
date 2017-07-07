-- module Scratch where
-- import Control.Monad.ST

-- data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show)

-- fresh :: ST Int Int
-- fresh = 

-- -- mlabel :: Tree a -> ST Int (Tree a)
-- -- mlabel (Leaf _ ) = do n
  
-- --import Distribution
-- --import Element
-- --import Inference

-- -- scratch :: IO ()
-- -- scratch = do
-- --   let greeting = observe (fromDistribution $
-- --                           select ["hi" ->> 0.3, "bye" ->> 0.7]) "bye"

-- --   putStrLn $ show $ probability greeting "hi"

-- --   let x = Unobserved $ Atomic $ constant 3
-- --   let y = Unobserved $ Atomic $ constant 5
-- --   let z = x + y

-- --   putStrLn $ show $ probability z 3
