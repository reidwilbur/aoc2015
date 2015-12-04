module Day3 (getNextAddress, housesWithDeliveries, housesWithDeliveries2) where
import qualified Data.Text as Text
import Data.List as List
import Data.Int as Int
import Data.Set as Set

housesWithDeliveries :: String -> Int.Int
housesWithDeliveries dirs = Set.size $ getDeliveredAddrs dirs

getDeliveredAddrs :: String -> Set.Set (Integer, Integer)
getDeliveredAddrs dirs = let initDels = Set.singleton (0,0)
                             foldDels (delSet, addr) dir =
                               ((Set.insert (getNextAddress dir addr) delSet), (getNextAddress dir addr))
                             (dels, _) = List.foldl foldDels (initDels, (0,0)) dirs
                         in dels

housesWithDeliveries2 :: String -> Int.Int
housesWithDeliveries2 dirs = let idxDirs = List.zipWith (\i d -> (i, d)) [1..] dirs
                                 santaDirs = List.map (\(_, d) -> d) $ List.filter (\(i, d) -> i `mod` 2 == 0) idxDirs
                                 roboDirs = List.map (\(_, d) -> d) $ List.filter (\(i, d) -> i `mod` 2 /= 0) idxDirs
                                 santaDels = getDeliveredAddrs santaDirs
                                 roboDels = getDeliveredAddrs roboDirs
                             in Set.size $ Set.union santaDels roboDels

getNextAddress :: Char -> (Integer, Integer) -> (Integer, Integer)
getNextAddress '^' (r,c) = (r+1,c)
getNextAddress '>' (r,c) = (r,c+1)
getNextAddress 'v' (r,c) = (r-1,c)
getNextAddress '<' (r,c) = (r,c-1)
getNextAddress e _ = error ("couldn't parse direction char '" ++ show(e) ++ "'")

