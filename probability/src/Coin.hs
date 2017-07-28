{-# LANGUAGE ExistentialQuantification #-}

module Coin where

import qualified Data.List as ListLib
import qualified Statistics.Distribution as DistLib
import qualified Statistics.Distribution.Normal as NormalLib
import GHC.Float

class Discrete a where
  pmf :: Eq b => a b -> b -> Double

class Continuous a where
  pdf :: (RealFloat b, Fractional b) => a b -> b -> Double
  cdf :: (RealFloat b, Fractional b) => a b -> b -> Double

-- Bernoulli Distribution
data Bernoulli a = Bernoulli Double a a

instance Discrete Bernoulli where
  pmf (Bernoulli p s f) x = if (x == s) then p else 1 - p

-- Select Distribution
data Select a = Select [(a, Double)]

instance Discrete Select where
  pmf (Select pairs) x = case ListLib.lookup x pairs of
                           (Just p) -> p
                           Nothing -> 0
                           
-- Normal Distribution
data Normal a = Normal a a

instance Continuous Normal where
  pdf (Normal mu sigma) x = DistLib.density (NormalLib.normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x)
  cdf (Normal mu sigma) x = DistLib.cumulative (NormalLib.normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x) 
