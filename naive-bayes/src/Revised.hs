module Revised where

import Text.ParserCombinators.Parsec
import Text.CSV
import qualified Statistics.Matrix as Mat
import Data.Either
import Data.Maybe

main :: IO ((Mat.Matrix, Mat.Vector))
main = do raw <- parseCSVFromFile "data/diabetes.csv"
          let csvRaw = head (rights [raw])
          let stringToDouble = \x -> read x :: Double
          let numData = map (\x -> map stringToDouble x) csvRaw
          let mat = Mat.fromRowLists numData
          let labels = Mat.column mat 8
          -- inefficient; better matrix slicing method?
          let featureVecs = Mat.fromColumns $ take 8 $ Mat.toColumns mat
          return (featureVecs, labels)

