
module Main where
import Text.ParserCombinators.Parsec
import Text.CSV
import Data.Either
import Data.Maybe
import qualified Data.Map as Map


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

    
processCSV :: CSV -> Bool
processCSV x = True


main :: IO ()
main = do x1 <- parseCSVFromFile "data/csv_test.csv"
          let result = (either (\x ->  False) (processCSV))  x1
          putStrLn (if result then "True" else "False")

