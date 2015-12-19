module Day10 (getNextSeq) where
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map.Strict as Map

getNextSeq :: String -> String
getNextSeq xs@(x:_) = (show $ List.length $ List.takeWhile (==x) xs) ++ [x] ++ (getNextSeq $ List.dropWhile (==x) xs)
getNextSeq [] = []

