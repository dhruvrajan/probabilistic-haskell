module Factor where

import Element
import Distribution
import Inference
import qualified Numeric.LinearAlgebra.HMatrix as HM
import Data.List

getParents :: Element a -> [Element a]
getParents (DiscreteAtomic d) = []
getParents (CPD1 p _ ) = [element p]
getParents (CPD2 p1 p2 _) = [element p1, element p2]

reduce :: Eq a => [(b, RV a)] -> [a]
reduce [] = []
reduce ((_, rv):xs) = union (values $ element rv) (reduce xs)

values :: Eq a => Element a -> [a]
values (DiscreteAtomic dist) = domain dist
values (CPD1 rv pairs) = reduce pairs
values (CPD2 rv1 rv2 pairs) = reduce pairs

rvalues :: Eq a => RV a -> [a]
rvalues = values . element

domains :: Eq a => [Element a] -> [[a]]
domains es = map values es

worlds :: [[a]] -> [[a]]
worlds [] = [[]]
worlds (xs:xss) = [(x:ys) | x <- xs, ys <- yss] where
  yss = worlds xss



a = Unobserved (DiscreteAtomic $ Bernoulli 0.4 "red" "green") []
b = Unobserved (DiscreteAtomic $ Bernoulli 0.5 "paper" "metal") []
c = Unobserved (DiscreteAtomic $ Bernoulli 0.6 "slate" "plastic") []
d = Unobserved (CPD1 a [("red", b), ("green", c)]) []

medium = Unobserved (DiscreteAtomic $ Constant "medium") []
pricey = Unobserved (DiscreteAtomic $ Constant "pricey") []
cheap = Unobserved (DiscreteAtomic $ Constant "cheap") []

cost = Unobserved (CPD2 b c [(("paper", "slate"), medium),
                             (("paper", "plastic"), cheap),
                             (("metal", "slate"), pricey),
                             (("metal", "plastic"), medium)]) []
