{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Probability where
import Control.Monad



type Distribution a = a -> Double

data Element a =
  Atomic (Distribution a) (Maybe a)
  | If (Element Bool) (Element a) (Element a) (Maybe a)
  | forall b. Apply (Element b) (b -> a) (Maybe a)
  | forall b. Chain (Element b) (b -> Element a) (Maybe a)

observe :: Element a -> a -> Element a
observe (Atomic dist _) obs    = Atomic dist $ Just obs
observe (If tst thn els _) obs = If tst thn els $ Just obs
observe (Apply emt fn _) obs   = Apply emt fn $ Just obs
observe (Chain emt fn _) obs   = Chain emt fn $ Just obs

unobserve :: Element a -> Element a
unobserve (Atomic dist _)    = Atomic dist $ Nothing
unobserve (If tst thn els _) = If tst thn els $ Nothing
unobserve (Apply emt fn _)   = Apply emt fn $ Nothing
unobserve (Chain emt fn _)   = Chain emt fn $ Nothing


-- unobserve :: Element a -> Element a
-- data Element a where
--   -- distribution, observed value
--   Atomic :: Distribution a -> Maybe a -> Element a
--   If     :: Element Bool -> Element a -> Element a -> Maybe a -> Element a
--   Apply  :: Element b -> (b -> a) -> (Maybe a) -> Element a
--   Chain  :: Element b -> (b -> Element a) -> (Maybe a) -> Element a

--instance Functor Element where
--  fmap f e = Apply e f Nothing


-- class Element a where
--   observe :: a -> b -> a
--   unobserve :: a -> a


-- data Atomic a = Atomic (Distribution a) (Maybe a)
-- data If a = If (Element Bool) (Element a) (Element a) (Maybe a)
-- data Apply a b = Apply (Element a) (a -> b) (Maybe b)

-- fromDistribution :: Distribution a -> Element a
-- fromDistribution dist = Atomic dist Nothing

-- flip :: Double -> Element Bool
-- flip p = fromDistribution $ \ x  -> case x of True ->  p
--                                               False -> 1 - p         


-- probability :: Eq a => Element a -> a -> Double
-- probability Atomic { distribution = d, observed = Nothing } val = d val
-- probability Atomic { distribution = d, observed = Just val} x
--   = if (x == val) then 1.0 else 0.0
-- probability If {tst = tst, thn = thn, els = els} val =
--   (probability tst True) * (probability thn val) +
--   (probability tst False) * (probability els val)

