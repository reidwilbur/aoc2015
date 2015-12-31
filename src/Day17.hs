module Day17 (getAllContainers, getNumMinContainers) where
import Debug.Trace
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

getAllContainers :: Int -> [Int] -> [[Int]]
getAllContainers vol cs = List.filter ((==vol) . List.sum) $ List.subsequences cs

getNumMinContainers :: Int -> [Int] -> [[Int]]
getNumMinContainers vol cs = let combos = getAllContainers vol cs
                                 minLen = List.foldl' (\m l -> min m $ List.length l) (maxBound :: Int) combos
                             in List.filter ((==minLen) . List.length) combos

