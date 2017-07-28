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



getFactor :: Eq a => Element a -> ([[a]], [a], HM.Matrix Double)
getFactor rv = (col, row, cpt) where
  col = worlds $ domains $ getParents $ rv
  row = values rv
  cpt = case rv of
    (DiscreteAtomic d) -> HM.fromLists [[pmf d x | x <- domain d]]
    (CPD1 rv1 pairs) -> HM.fromLists [[probability var d | d <- values rv] | (val, var) <- pairs]
--    (CPD2 rv1 rv2 pairs) -> [[probability v d] | (x, v) <- pairs, d <- values rv]
  
  


a = Unobserved (DiscreteAtomic $ Bernoulli 0.2 "red" "green") []
b = Unobserved (DiscreteAtomic $ Bernoulli 0.3 "paper" "metal") []
c = Unobserved (DiscreteAtomic $ Bernoulli 0.6 "slate" "plastic") []
d = Unobserved (CPD1 a [("red", b), ("green", c)]) []

dfactor = getFactor $ element d

medium = Unobserved (DiscreteAtomic $ Constant "medium") []
pricey = Unobserved (DiscreteAtomic $ Constant "pricey") []
cheap = Unobserved (DiscreteAtomic $ Constant "cheap") []

cost = Unobserved (CPD2 b c [(("paper", "slate"), medium),
                             (("paper", "plastic"), cheap),
                             (("metal", "slate"), pricey),
                             (("metal", "plastic"), medium)]) []
