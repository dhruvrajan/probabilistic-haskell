
module Main where
import Text.ParserCombinators.Parsec
import Text.CSV
import Data.Either
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List



-- Statistics Helper Functions
mean :: [Float] -> Float
mean ns = (sum ns) / (fromIntegral (length ns) :: Float)

stdev :: [Float] -> Float
stdev ns = result
  where
    avg = mean(ns)
    variance = (sum [(n - avg)^2 | n <- ns]) / (fromIntegral (if length ns - 1 == 0 then 1 else length ns - 1) :: Float)
    result = sqrt variance
calcProb :: Float -> Float -> Float -> Float
calcProb x m s = result
  where
    e = exp (-((x-m)^2)/(2 * (s^2)))
    result = (1 / (sqrt(2*pi) * s)) * e

-- data transformations
stringToFloat :: String -> Float
stringToFloat s = read s :: Float

getNumericData :: CSV -> [[Float]]
getNumericData [] = []
getNumericData (x:xs) = map stringToFloat x : getNumericData xs

getClassVecs :: [[Float]] -> [(Int, [Float])]
getClassVecs [] = []
getClassVecs (x:xs) = (round (last x), take (length x - 1) x) : getClassVecs xs


addToClass :: (Int, [Float]) -> [(Int, [[Float]])] -> [(Int, [[Float]])]
addToClass (x, ys) [] = [(x, [ys])]
addToClass (x, ys) ((z1,z2) : zs) = if (x == z1)
                                    then (z1, ys : z2) : zs
                                    else (z1,  z2) : (addToClass (x, ys) zs)
                                                 
    
separateByClass :: [(Int, [Float])] -> [(Int, [[Float]])] -> [(Int, [[Float]])]
separateByClass [] ys = ys
separateByClass (x:xs) ys = separateByClass xs (addToClass x ys)


getCSVData :: IO([Record])
getCSVData = do d <- parseCSVFromFile "data/diabetes.csv"
                let y = head (rights [d])
                return $ take (length y - 1) y

transpose :: [Float] -> [[Float]]
transpose [] = []
transpose (x:xs) = [x] : transpose xs

zipWithCons :: [[Float]] -> [[Float]]
zipWithCons [] = []
zipWithCons [x] = transpose x
zipWithCons (x1 : xs) = zipWith (:) x1 (zipWithCons xs)

summarizeData :: [[Float]] -> [(Float, Float)]
summarizeData xs = result
  where
    zipped = zipWithCons xs
    result = [(mean row, stdev row) | row <- zipped]

summarizeByClass :: [[Float]] -> [(Int, [(Float, Float)])]
summarizeByClass xs = result
  where 
    byClass = separateByClass (getClassVecs xs) []
    result = map (\(x1,x2) -> (x1, summarizeData x2)) byClass

classProbability :: (Int, [(Float, Float)]) -> [Float] -> Float
classProbability (x, []) vec = 1
classProbability (x, ((mu, st):ss)) (v:vs) =  (calcProb v mu st) * classProbability (x, ss) vs

calculateClassProbabilities :: [(Int, [(Float, Float)])] -> [Float] -> [(Int, Float)]
calculateClassProbabilities [] _ = []
calculateClassProbabilities ((c, cs):xs) vec = (c, classProbability (c, cs) vec) :  calculateClassProbabilities xs vec


predict :: [(Int, [(Float, Float)])] -> [Float] -> Int
predict summ vec = result
  where
    classProbs = calculateClassProbabilities summ vec
    sorted = List.sortOn (\(x, y) -> -y) classProbs
    (result, _) = head sorted

getPredictions :: [(Int, [(Float, Float)])] -> [[Float]] -> [Int]
getPredictions _ [] = []
getPredictions  summaries (t:ts) = predict summaries t : getPredictions summaries ts
                                               
processCSV :: CSV -> Bool
processCSV x = True

main :: IO ()
main = do raw <- getCSVData
          let numeric = getNumericData raw
          let classified = getClassVecs numeric
          let dataSummary = summarizeData numeric
          let classSummary = summarizeByClass numeric
          
          putStrLn "finished"

-- let summaries = [(0,[(1,0.5)]), (1,[(20,5.0)])]
-- let tests = [[1.1, 1], [1.91, 1]]
-- getPredictions summaries tests
