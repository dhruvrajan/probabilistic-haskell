{-# LANGUAGE ExistentialQuantification #-}
module Distribution where

import qualified Data.List as ListLib
import qualified Statistics.Distribution as DistLib
import qualified Statistics.Distribution.Normal as NormalLib
import GHC.Float

class Discrete a where
  pmf :: Eq b => a b -> b -> Double
  domain :: Eq b => a b -> [b]

class Continuous a where
  pdf :: (RealFloat b, Fractional b) => a b -> b -> Double
  cdf :: (RealFloat b, Fractional b) => a b -> b -> Double

-- Bernoulli Distribution
data Bernoulli a = Bernoulli Double a a

instance Discrete Bernoulli where
  pmf (Bernoulli p s f) x = if (x == s) then p else 1 - p
  domain (Bernoulli _ s f)  = [s, f]

-- Select Distribution
data Select a = Select [(a, Double)]

instance Discrete Select where
  pmf (Select pairs) x = case ListLib.lookup x pairs of
                           (Just p) -> p
                           Nothing -> 0                           
  domain (Select pairs) = map fst pairs

-- Constant Distribution
data Constant a = Constant a

instance Discrete Constant where
  pmf (Constant c) x = if (c == x) then 1.0 else 0.0
  domain (Constant c) = [c]
  
-- Normal Distribution
data Normal a = Normal a a

instance Continuous Normal where
  pdf (Normal mu sigma) x = DistLib.density (NormalLib.normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x)
  cdf (Normal mu sigma) x = DistLib.cumulative (NormalLib.normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x) 
