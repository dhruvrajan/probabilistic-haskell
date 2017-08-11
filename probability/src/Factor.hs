module Factor where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Debug.Trace

type KeyType = Bool -- just dealing with bools for now

-- | A lookup table. Values are of type a, and
-- the input is a vector of KeyType
--type Table a = M.Map (V.Vector KeyType) a

--data Factor = Factor (V.Vector Int) (Table Double) deriving (Eq, Show)
type Table a = M.Map [KeyType] a

data Factor = Factor [Int] (Table Double) deriving (Eq, Show)

-- | Get the list of ids from a factor
ids :: Factor -> [Int]
ids (Factor ids _) = ids

-- | Get the table of a factor
table :: Factor -> Table Double
table (Factor _ table) = table

-- | Convert a [([KeyType], a)] to [(Vector Keytype, a)]
-- TODO: Should be a Fold
_listKeysToVec :: [([KeyType], a)] -> [([KeyType], a)]
_listKeysToVec [] = []
_listKeysToVec ((ks, a):xs) = (ks, a) : _listKeysToVec xs

-- | Generate all possible boolean vectors of a given size
boolPermutations :: Int -> [[Bool]]
boolPermutations 0 = [[]]
boolPermutations n = let ps = boolPermutations (n - 1)
                      in union  [True:cs | cs <- ps] [False:cs | cs <- ps]

-- | Create a table from a [([KeyType], Double)]
createTable :: [([KeyType], a)] -> (Table a)
createTable ks = M.fromList $ _listKeysToVec ks

createFactor :: [Int] -> [([KeyType], Double)] -> Factor
createFactor ids ks = Factor (ids) $ createTable ks

-- | Deletes the element at some index from a vector.
-- TODO: Error Handling if index is invalid?
deleteAt :: Int -> [a] -> [a]
deleteAt idx v = let (l, r) = splitAt idx v in  l ++  (tail r)

-- | Filter a distribution on some value of some variable
filterFactor :: Factor -> Int -> KeyType -> Maybe Factor
filterFactor (Factor ids table) varId val = do
  index <- elemIndex varId ids
  let filterTable = M.filterWithKey (\k v -> k !! index == val) table
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
-- TODO: Manage Division by Zero
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


-- TODO: RE-EVALUATE POINTWISE PRODUCT; BETTER IMPLEMENTATION
splitPerm :: [Int] -> [Int] -> [Bool] -> ([Bool], [Bool])
splitPerm f1 f2 ps = (f1Perm, f2Perm) where
  newKeys = reverse $ sort $ union f1 f2
  enumPs = zip newKeys ps
  f1PermPairs = filter (\(nid, p) -> elem nid f1) enumPs
  f1Perm = map (\(x, y) -> y) f1PermPairs

  f2PermPairs = filter (\(nid, p) -> elem nid f2) enumPs
  f2Perm = map(\(x, y) -> y) f2PermPairs
  

pointwiseProduct :: Factor -> Factor -> Factor
pointwiseProduct f1 f2 = f3 where
  table1 = table f1
  table2 = table f2
  newKeys = reverse $ sort $ union (ids f1) (ids f2)
  perms =  boolPermutations $ length newKeys
  pairs = map (\x -> (x, splitPerm (ids f1) (ids f2) x)) perms
  evalProbs = map (\(x, (f1_v, f2_v)) -> (x, (fromJust $ M.lookup f1_v table1) *
                                         (fromJust $ M.lookup f2_v table2))) pairs

  table3 = M.fromList evalProbs
  f3 = Factor newKeys table3


-- | TODO: RE-EVALUATE sumOut; BETTER IMPLEMENTATION
sumOut :: Int -> Factor -> Factor
sumOut nodeId (Factor vars table)  = normalize $ Factor newVars newValues where
  targetIdx = fromJust $ elemIndex nodeId vars
  -- Target is True
  isTrue = M.filterWithKey (\k _ -> k !! targetIdx == True) table
  isTrue' = M.mapKeys (\k1 -> deleteAt targetIdx k1) isTrue

  -- Target is False
  isFalse = M.filterWithKey (\k _ -> k !! targetIdx == False) table
  isFalse' = M.mapKeys (\k1 -> deleteAt targetIdx k1) isFalse

  -- TODO: Proper Maybe Handling
  newValues = M.mapWithKey (\k v -> v + (fromJust $ M.lookup k isFalse')) isTrue'

  --newKeys = Map.mapKeys (\k1 -> deleteAt targetIdx k1) newValues
  newVars = delete nodeId vars

  

-- -- | Compute the pointwise - product of two factors
-- pointwise :: Factor -> Factor -> Factor
-- pointwise (Factor ids1 table1) (Factor ids2 table2) = f3 where
--   newIds = reverse $ sort $ union ids1 ids2
--   id2idx = zip newIds [1..] -- basis for indices
--   idx2id = zip [1..] newIds

--   -- find the list of ids for each factor in terms of the new basis
--   ids1Idx = map (\x -> fromJust $ lookup x id2idx) ids1 -- TODO: assuming just
--   ids2Idx = map (\x -> fromJust $ lookup x id2idx) ids2 -- TODO: assuming just

--   -- find common / uncommon ids in terms of new basis
--   commonIdx = reverse $ sort $ intersect ids1Idx ids2Idx
--   onlyF1Idx = reverse $ sort $ ids1Idx \\ commonIdx
--   onlyF2Idx = reverse $ sort $ ids2Idx \\ commonIdx

--   -- Get the new table keys
--   enumNewKeys = [zip x [1..] | x <- boolPermutations $ length newIds]
--   commons = [filter (\x -> elem x commonIdx) l | l <- enumNewKeys]
--   onlyF1 = [filter (\x -> elem x onlyF1Idx) l | l <- enumNewKeys]
--   onlyF2 = [filter (\x -> elem x onlyF2Idx) l | l <- enumNewKeys]

--   f1Query = [f]






