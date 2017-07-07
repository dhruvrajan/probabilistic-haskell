module Distribution where
import Data.List (lookup)
import Data.Maybe

data Distribution a = Continuous (a -> Double) | Discrete [(a, Double)]

(->>) :: a -> b -> (a, b)
x ->> y = (x, y)

bernoulli :: Double -> Distribution Bool
bernoulli p = Discrete [(True, p), (False, 1-p)]

constant :: Eq a => a -> Distribution a
constant val = Discrete [(val, 1.0)]

select :: Eq a => [(a, Double)] -> Distribution a
select pairs = Discrete pairs

pdf :: Eq a =>  Distribution a -> (a -> Double)
pdf (Continuous fn) = fn
pdf (Discrete pairs) =
  \x ->
    if sum [snd y | y <- pairs] == 1.0
    then (\x -> if isJust x then fromJust x else 0.0) $ lookup x pairs
    else error "weights must sum to one"

