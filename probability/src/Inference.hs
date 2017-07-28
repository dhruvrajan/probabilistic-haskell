{-# LANGUAGE ExistentialQuantification #-}
module Inference where

-- import Element
-- import Data.Dynamic
-- import Distribution

-- probability :: Eq a => RV a -> a -> Double
-- probability (Unobserved (DiscreteAtomic dist) _) val = pmf dist val
-- --probability (Unobserved (ContinuousAtomic dist) _) val = pdf dist val
-- probability (Unobserved (CPD1 rv (((b), e):xs)) _) val =
--   (probability e val) * (probability rv b) +
--   (probability (Unobserved (CPD1 rv xs) []) val) 
-- probability (Unobserved (If tst thn els) _) val =
--   (probability tst True)  * (probability thn val) +
--   (probability tst False) * (probability els val)
-- probability (Observed (_) x _) val = if (x == val) then 1.0 else 0.0


-- getParents :: Element a -> [Variable]
-- getParents (DiscreteAtomic d) = []
-- getParents (If p _ _ ) = [Var p]
-- getParents (CPD1 p _ ) = [Var p]
-- getParents (CPD2 p1 p2 _) = [Var p1, Var p2]
-- getParents (CPD3 p1 p2 p3 _) = [Var p1, Var p2, Var p3]
-- getParents (Apply p _) = [Var p]
-- getParents (Chain p _) = [Var p]

-- getParentsList ::  RV a -> [Variable]
-- getParentsList rv = getParents $ element rv


-- data ListVal = forall a . ListVal [Value]

-- fromListVal :: ListVal -> [Value]
-- fromListVal (ListVal x) = x

-- combs :: [ListVal] -> [ListVal]
-- combs [] = [ListVal []]
-- combs ((ListVal xs):zs) = [ListVal ((Val x) : ys) | x <- xs, (ListVal ys) <- yss] where
--   yss = (combs zs)


-- -- getParentDomain :: RV a -> [ListVal]
-- -- getParentDomain rv = cs where
-- --   parents = getParentsList rv
-- --   parentDomains = [ListVal $ rvValuesVal p | (Var p) <- parents]
-- --   cs = combs parentDomains
  
-- -- getParentDomain :: RV a -> [[Value]]
-- -- getParentDomain rv = x where
-- --   parents = getParentsList rv
-- --   parentDomains = [Val $ rvValues p | (Var p) <- parents]

