module Factor where

import qualified Data.Vector as V
import qualified Data.Map as M



type KeyType = Bool -- just dealing with bools for nowx

-- | A lookup table. Values are of type a, and
-- the input is a vector of KeyType
type Table a = M.Map (V.Vector KeyType) a

data Factor = Factor (V.Vector Int) (Table Double)

deleteAt :: Int -> V.Vector a -> V.Vector a
deleteAt idx v = let (l, r) = V.splitAt idx v in (V.++) l $  V.tail r

marginalize :: Factor -> Int -> KeyType -> Maybe Factor
marginalize (Factor ids table) varId val = do
  index <- V.elemIndex varId ids
  let filterTable = M.filterWithKey (\k v -> k V.! index == val) table
  let fixKeys = M.mapKeys (deleteAt index) table
  let fixIndices = deleteAt index ids
  return $ Factor fixIndices fixKeys
