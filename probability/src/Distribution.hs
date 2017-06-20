module Distribution where
import Data.List (lookup)
import Data.Maybe

type Distribution a = a -> Double

(->>) :: a -> b -> (a, b)
x ->> y = (x, y)

bernoulli :: Double -> Distribution Bool
bernoulli p = \x -> case x of True -> p
                              False -> 1 - p
constant :: Eq a => a -> Distribution a
constant val = \x -> if x == val then 1.0 else 0.0

select :: Eq a => [(a, Double)] -> Distribution a
select pairs =
  \x ->
    if sum [snd y | y <- pairs] == 1.0
    then (\x -> if isJust x then fromJust x else 0.0) $ lookup x pairs
    else error "weights must sum to one"

