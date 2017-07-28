{-# LANGUAGE ExistentialQuantification #-}
module Element where
import Control.Monad
import Distribution
import Data.List

data Element a =
  forall d. Discrete d => DiscreteAtomic (d a)
  | CPD1 (RV a) [(a, RV a)]
  | CPD2 (RV a) (RV a) [((a, a), RV a)]

data RV a = Observed (Element a) a [RV a] | Unobserved (Element a) [RV a]

element :: RV a -> Element a
element (Observed e _ _) = e
element (Unobserved e _) = e

--probability :: Eq a => RV a -> a -> Double
--probability (Observed _ obs _) val = if val == obs then 1.0 else 0.0






























--probability (Unobserved _ )

-- data Element a =
--   forall d . (Discrete d, Eq a) => DiscreteAtomic (d a)
--   -- | forall d . (Continuous d, RealFloat a) => ContinuousAtomic (d a)
--   | Eq a => If (RV Bool) (RV a) (RV a)
--   | forall b. (Eq b, Eq a) => CPD1 (RV b) [((b), RV a)]
--   | forall b c. (Eq b, Eq c, Eq a) => CPD2 (RV b) (RV c) [((b, c), RV a)]
--   | forall b c d. (Eq b, Eq c, Eq d, Eq a) => CPD3 (RV b) (RV c) (RV d) [((b, c, d), RV a)]
--   | forall b. (Eq a) => Apply (RV b) (b -> a)
--   | forall b. (Eq a) => Chain (RV b) (b -> a) 

-- data Variable = forall a . Var (RV a)
-- data Value = forall a . Val a
-- data RV a = Eq a => Observed (Element a) a [Variable]
--   | Eq a => Unobserved (Element a) [Variable]

-- extractValues :: Eq b => [(a, RV b)] -> [b]
-- extractValues [] = []
-- extractValues ((_, rv):xs) = union (rvValues rv) (extractValues xs)

-- elementValues :: Eq a => Element a -> [a]
-- elementValues (DiscreteAtomic dist) = domain dist
-- elementValues (If _ thn els) = union (rvValues thn) (rvValues els)
-- elementValues (CPD1 _ pairs) = extractValues pairs
-- elementValues (CPD2 _ _ pairs) = extractValues pairs
-- elementValues (CPD3 _ _ _ pairs) = extractValues pairs

-- rvValues :: Eq a => RV a -> [a]
-- rvValues  = elementValues . element 

-- rvValuesVal :: Eq a => RV a -> [Value]
-- rvValuesVal rv = map Val $ rvValues rv

-- element :: RV a -> Element a
-- element (Observed e _ _) = e
-- element (Unobserved e  _) = e

-- observe :: RV a -> a -> RV a
-- observe (Observed e _ ps) val = Observed e val ps
-- observe (Unobserved e ps) val = Observed e val ps

-- unobserve :: RV a -> RV a
-- unobserve (Observed e _ ps) = Unobserved e ps
-- unobserve (Unobserved e ps) = Unobserved e ps

instance Show a => Show (Element a) where
  show (DiscreteAtomic dist) = "DiscreteAtomic"
  show (CPD1 _ _) = "CPD 1"
  show (CPD2 _ _ _) = "CPD 2"
--  show (ContinuousAtomic dist) = "ContinuousAtomic"
--  show (If _ _ _ ) = "If"
--  show (Apply _ _) = "Apply"
--  show (Chain _ _) = "Chain"

instance Show a => Show (RV a) where
  show (Observed e val ps) = "Observed" ++ (show e) ++ (show val)
  show (Unobserved e ps)  = "Unobserved (" ++ (show e) ++ ") "

        
