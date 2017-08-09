module Factor where

import qualified Data.Vector as V
import qualified Data.Map as M



type KeyType = Bool -- just dealing with bools for now

-- | A lookup table. Values are of type a, and
-- the input is a vector of KeyType
type Table a = M.Map (V.Vector KeyType) a

data Factor = Factor (V.Vector Int) (Table Double)

deleteAt :: Int -> V.Vector a -> V.Vector a
deleteAt idx v = let (l, r) = V.splitAt idx v in (V.++) l $  V.tail r


marginalize :: Factor -> Int -> KeyType -> Maybe Factor
marginalize (Factor ids table) varId val = marginalized
  where
    index = V.elemIndex varId ids
    filterTable = index >>= \i -> Just $ M.filterWithKey (\k v -> (k V.! i == val)) table
    fixKeys = filterTable >>= \t -> index >>= \i -> Just $ M.mapKeys (deleteAt i) t
    fixIndices = index >>= \i -> Just $ deleteAt i ids
    marginalized = fixIndices >>= \ids -> fixKeys >>= \ks -> Just $ Factor ids ks
