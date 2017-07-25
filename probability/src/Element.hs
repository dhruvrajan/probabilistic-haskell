{-# LANGUAGE ExistentialQuantification #-}
module Element where
import Control.Monad
import Data.Maybe
import Data.List
import Distribution

data Element a =
  forall d . Distribution d => Atomic d
  | If (RV Bool) (RV a) (RV a)
  | forall b. Apply (RV b) (b -> a)
  | forall b. Chain (RV b) (b -> a)

data RV a = Observed (Element a) a | Unobserved (Element a)  

-- instance Show a => Show (Element a) where
--   show (Atomic dist) = "Atomic"
--   show (Apply _ _) = "Apply"
--   show (Negate rv) = "Negate (" ++ (show rv)
--   show (ScalarAdd rv val) = "ScalarAdd (" ++ (show rv) ++ (show val) ++ ") "
--   show (Sum rv1 rv2) = "Sum ("  ++ (show rv1) ++ (show rv2) ++ ") "
--   show (RVProduct rv1 rv2) = "RVProduct (" ++ (show rv1) ++ (show rv2) ++ ") "
--   show (ScalarProduct rv n) = "ScalarProduct (" ++ (show rv) ++ (show n) ++ ") "
--   show (Power rv 
--        n) = "Power (" ++ (show rv) ++ (show n) ++ ") "

-- data RV a = Observed (Element a) a | Unobserved (Element a)

-- instance Show a => Show (RV a) where
--   show (Observed e val) = "Observed" ++ (show e) ++ (show val)
--   show (Unobserved e) = "Unobserved (" ++ (show e) ++ ") "
  
-- instance Num a => Num (RV a) where
--   rv1 + rv2 = Unobserved $ Sum rv1 rv2
--   rv1 * rv2 = Unobserved $ RVProduct rv1 rv2
--   negate rv = Unobserved $ Negate rv

-- observe :: RV a -> a -> RV a
-- observe (Observed e _) val = Observed e val
-- observe (Unobserved e) val = Observed e val

-- unobserve :: RV a -> RV a
-- unobserve (Observed e _) = Unobserved e
-- unobserve (Unobserved e) = Unobserved e

-- fromDistribution :: Distribution a -> RV a
-- fromDistribution dist = Unobserved $ Atomic dist

-- coinFlip :: Double -> RV Bool
-- coinFlip p = fromDistribution $ bernoulli p

-- cpd :: Eq a => Element a -> [(a, Element a)] -> Element a
-- cpd prior pairs = Chain (Unobserved prior) fn
--   where
--     fn =
--       \x ->
--         (\x -> if isJust x then fromJust x else error "domain not specified") $ lookup x pairs


        
