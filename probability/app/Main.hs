module Main where

-- import Data.Monoid
-- import Control.Monad.Writer
-- import Control.Monad.State

-- type Stack = [Int]

-- pop' :: Stack -> (Int, Stack)
-- pop' (x:xs) = (x,xs)

-- push' :: Int -> Stack -> ((), Stack)
-- push' a xs = ((), a:xs)

-- stackManip' :: Stack -> (Int, Stack)
-- stackManip' stack = let
--   ((), newStack1) = push' 3 stack
--   (a, newStack2) = pop' newStack1
--   in pop' newStack2

-- pop :: State Stack Int
-- pop = state $ \(x:xs) -> (x, xs)

-- push :: Int -> State Stack ()
-- push a = state $ \xs -> ((), a:xs)

-- stackManip :: State Stack Int
-- stackManip = do
--   push 3
--   a <- pop
--   pop 


-- newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

-- toDiffList :: [a] -> DiffList a
-- toDiffList xs = DiffList (xs++)

-- fromDiffList :: DiffList a -> [a]
-- fromDiffList (DiffList f) = f []

-- instance Monoid (DiffList a) where
--   mempty = DiffList (\xs -> xs)
--   (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f . g $ xs)


-- logNumber :: Int -> Writer [String] Int
-- logNumber x = writer (x, ["Got number: " ++ show x])

-- multWithLog :: Writer [String] Int
-- multWithLog = do
--   a <- logNumber 3
--   b <- logNumber 5
--   tell ["Gonna multiply now"]
--   return (a*b)

-- isBigGang :: Int -> (Bool, [String])
-- isBigGang x = (x > 9, ["Compared gang size to 9."])

-- type Food = String
-- type Price = Sum Int

-- addDrink :: Food -> (Food, Price)
-- addDrink "beans" = ("milk", Sum 25)
-- addDrink "jerky" = ("whiskey", Sum 99)
-- addDrink _ = ("beer", Sum 30)


-- applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

-- main :: IO ()
-- main = do
--   putStrLn . show $ isBigGang 3




import Distribution
import Element
import Inference

select1 :: Distribution String
select1 "Hello, world!" = 0.6
select1 "Howdy, universe!" = 0.4

select2 :: Distribution String
select2 "Hello, world!" = 0.2
select2 "Howdy, universe!" = 0.8

main :: IO ()
main  = do
  -- Set Up
  let sunnyToday = flip 0.2
  let greetingToday1 = If sunnyToday (fromDistribution select1)
                          (fromDistribution select2) Nothing
  let sunnyTomorrow = If sunnyToday (flip 0.8) (flip 0.05) Nothing
  let greetingTomorrow = If sunnyTomorrow (fromDistribution select1)
                         (fromDistribution select2) Nothing
  -- Predict
  let prediction = probability greetingToday1 "Hello, world!"
  putStrLn("Today's greeting is \"Hello, world!\" " ++
           "with probability " ++ (show prediction) ++ ".")

  -- Infer
  let greetingToday2 = observe greetingToday1 "Hello, world!"
  let inference = probability sunnyToday True
  putStrLn ("If today's greeting is \"Hello, world!\", today's " ++
          "weather is sunny with probability " ++ (show inference) ++ ".")

  -- Learn And Predict
  let greetingToday3 = observe greetingToday2 "Hello, world!"
  let result = probability greetingTomorrow "Hello, world!"
  putStrLn("If today's greeting is\"Hello, world!\", " ++
          "tomorrow's greeeting will be \"Hello, world!\" " ++
          "with probability " ++ (show result) ++ ".")
