module Day13 (parseInput, CostMap, getHappiness, getMaxHappiness) where
import Debug.Trace
import Data.List ((!!))
import Data.Map ((!))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type CostMap = Map.Map String (Map.Map String Int)

getMaxHappiness :: CostMap -> (Int, [String])
getMaxHappiness cmap = let configs = List.permutations $ Set.toList $ Map.keysSet cmap
                           hcfg = List.map (\c -> (getHappiness cmap c, c)) configs
                       in foldr (\e@(h, c) m@(mh, _) -> if h > mh then e else m) (minBound, []) hcfg

getHappiness :: CostMap -> [String] -> Int
getHappiness cmap order =
  let len = List.length order
      nbrs i = List.map ((!!) order)
                 [(i - 1) `mod` len, (i + 1) `mod` len]
      cost i = let name = order !! i
                   nbrmap = cmap ! name
                   nbrcst = List.map ((!) nbrmap) $ nbrs i
               in List.sum nbrcst
  in List.sum $ List.map (cost) [0..len - 1]

parseLine :: String -> (String, (String, Int))
parseLine s = let words = List.words $ List.init s
                  extr = (words !! 0, words !! 2, words !! 3, List.last words)
              in case extr of
                   ((s,"gain",h,t)) -> (s, (t, read h))
                   ((s,"lose",h,t)) -> (s, (t, ((-1) * read h)))
                   _ -> error ("Can't parse " ++ s)

parseInput :: [String] -> CostMap
parseInput ss = let entries = List.foldr (\s es -> parseLine s : es) [] ss
                    addCost m (t, h) = Map.insert t h m
                in List.foldr (\(s, c) cmap ->
                    Map.insert s (addCost (Map.findWithDefault (Map.empty) s cmap) c) cmap)
                  Map.empty entries

