module Factor where

import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

type KeyType = Bool -- just dealing with bools for nowx

-- | A lookup table. Values are of type a, and
-- the input is a vector of KeyType
type Table a = M.Map (V.Vector KeyType) a

data Factor = Factor (V.Vector Int) (Table Double) deriving (Eq, Show)

-- | Get the list of ids from a factor
ids :: Factor -> V.Vector Int
ids (Factor ids _) = ids

-- | Get the table of a factor
table :: Factor -> Table Double
table (Factor _ table) = table



-- | Convert a [([KeyType], a)] to [(Vector Keytype, a)]
-- TODO: Should be a Fold
_listKeysToVec :: [([KeyType], a)] -> [(V.Vector KeyType, a)]
_listKeysToVec [] = []
_listKeysToVec ((ks, a):xs) = (V.fromList ks, a) : _listKeysToVec xs

-- | Create a table from a [([KeyType], Double)]
createTable :: [([KeyType], a)] -> (Table a)
createTable ks = M.fromList $ _listKeysToVec ks

createFactor :: [Int] -> [([KeyType], Double)] -> Factor
createFactor ids ks = Factor (V.fromList ids) $ createTable ks

-- | Deletes the element at some index from a vector.
-- TODO: Error Handling if index is invalid?
deleteAt :: Int -> V.Vector a -> V.Vector a
deleteAt idx v = let (l, r) = V.splitAt idx v in (V.++) l $  V.tail r

-- | Filter a distribution on some value of some variable
filterFactor :: Factor -> Int -> KeyType -> Maybe Factor
filterFactor (Factor ids table) varId val = do
  index <- V.elemIndex varId ids
  let filterTable = M.filterWithKey (\k v -> k V.! index == val) table
  let fixKeys = M.mapKeys (deleteAt index) filterTable
  let fixIndices = deleteAt index ids
  return $ Factor fixIndices fixKeys

-- | Add two factors
add :: Factor -> Factor -> Maybe Factor
add (Factor ids1 table1) (Factor ids2 table2) = do
  (t1, t2) <- check table1 table2
  let sumTable = M.unionWith (+) t1 t2 -- TODO: check that keys match, somehow
  return  $ Factor ids1 sumTable
  where
    check = \t1 t2 ->
      if (M.size t1 == M.size t2) && -- tables have an equal # elements
         (ids1 == ids2) && -- the key ids match
         (M.keys t1 == M.keys t2)
      then Just (t1, t2)
      else Nothing

-- | Normalize a factor so that all probabilities sum to 1
normalize :: Factor -> Factor
normalize (Factor ids table) = Factor ids $ M.map (\v -> v / sum) table where
  sum = foldr (+) 0 table

-- | Compute the marginal distribution for some variable
-- by setting it equal to some value 
marginalize :: Factor -> Int -> Maybe Factor
marginalize factor varId = do
  filterTrue <- filterFactor factor varId True
  filterFalse <- filterFactor factor varId False
  sum <- add filterTrue filterFalse
  return sum

