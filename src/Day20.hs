module Day20 (getPresentsIdxAtLeast, getPresents, getPresents2, genFactors) where
import Debug.Trace
import qualified Data.List as List

genFactors :: Int -> [Int]
genFactors idx = let idxs = [1..floor $ sqrt $ fromIntegral idx]
                     smalldivs = List.filter ((==) 0 . mod idx) idxs
                     largedivs = List.map (\d -> idx `div` d ) $ List.filter (\d -> d^2 /= idx) smalldivs
                 in smalldivs ++ largedivs

getPresents :: Int -> Int
getPresents = (*)10 . List.sum . genFactors

getPresents2 :: Int -> Int
getPresents2 addr = (*) 11 $ List.sum $ List.filter (\f -> addr `div` f <= 50) $ genFactors addr

getPresentsIdxAtLeast :: (Int -> Int) -> Int -> Int -> (Int, Int)
getPresentsIdxAtLeast getPresents startidx presents =
  List.head $ List.dropWhile ((>) presents . snd) $ List.map (\a -> (a, getPresents a)) [startidx..]

