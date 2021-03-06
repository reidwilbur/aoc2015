module Day1 (getFloor, firstBasement) where
import Data.List

decode :: Char -> Integer
decode '(' = 1
decode ')' = -1
decode e = error $ "unexpected input: " ++ show e

getFloor :: [Char] -> Integer
getFloor ps = foldl (\z p -> z + (decode p)) 0 ps

firstBasement :: [Char] -> Integer
firstBasement ps = let idxFloors = zipWith (\i floor -> (i, floor)) [0..] (scanl (\z p -> z + decode p) 0 ps)
                       dropped = dropWhile (\(i, floor) -> floor >= 0) idxFloors
                   in case dropped of
                     (i,floor):xs ->i
                     [] -> error "never goes into basement"

