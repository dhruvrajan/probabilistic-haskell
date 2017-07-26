{-# LANGUAGE ExistentialQuantification #-}
module Element where
import Control.Monad
import Distribution

data Element a =
  forall d. Discrete d a => DiscreteAtomic d
  | forall d. Continuous d a => ContinuousAtomic d
  | If (RV Bool) (RV a) (RV a)
  | forall b. Apply (RV b) (b -> a)
  | forall b. Chain (RV b) (b -> a)

data RV a = Observed (Element a) a | Unobserved (Element a)

observe :: RV a -> a -> RV a
observe (Observed e _) val = Observed e val
observe (Unobserved e) val = Observed e val

unobserve :: RV a -> RV a
unobserve (Observed e _) = Unobserved e
unobserve (Unobserved e) = Unobserved e

instance Show a => Show (Element a) where
  show (DiscreteAtomic dist) = "DiscreteAtomic"
  show (ContinuousAtomic dist) = "ContinuousAtomic"
  show (If _ _ _ ) = "If"
  show (Apply _ _) = "Apply"
  show (Chain _ _) = "Chain"

instance Show a => Show (RV a) where
  show (Observed e val) = "Observed" ++ (show e) ++ (show val)
  show (Unobserved e) = "Unobserved (" ++ (show e) ++ ") "
  
-- instance Num a => Num (RV a) where
--   rv1 + rv2 = Unobserved $ Sum rv1 rv2
--   rv1 * rv2 = Unobserved $ RVProduct rv1 rv2
--   negate rv = Unobserved $ Negate rv


-- coinFlip :: Double -> RV Bool
-- coinFlip p = fromDistribution $ bernoulli p

-- cpd :: Eq a => Element a -> [(a, Element a)] -> Element a
-- cpd prior pairs = Chain (Unobserved prior) fn
--   where
--     fn =
--       \x ->
--         (\x -> if isJust x then fromJust x else error "domain not specified") $ lookup x pairs


        
