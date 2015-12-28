module Day14 (Deer(Deer), parseDeer, getDistance, getWinner, getWinner2) where
import Debug.Trace
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Function as Fn
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Deer = Deer {name :: String
                  , speed :: Int
                  , runtime :: Int
                  , resttime :: Int} 
                 deriving (Show, Eq, Ord)

parseDeer :: String -> Deer
parseDeer ss = let words = List.words ss
               in case words of
                    (n:_:_:s:_:_:t:_:_:_:_:_:_:rt:_) -> Deer n (read s) (read t) (read rt)
                    _ -> error ("Can't parse" ++ ss)

getDistance :: Deer -> Int -> Int
getDistance deer seconds = let t = runtime deer + resttime deer
                               d = seconds `div` t
                               r = seconds `mod` t
                               rclamp = if r < runtime deer then r else runtime deer
                           in (d * runtime deer * speed deer) + (rclamp * speed deer)

getWinner :: [Deer] -> Int -> (String, Int)
getWinner ds seconds = List.last $ List.sortBy (Ord.compare `Fn.on` snd) $ List.map (\d -> (name d, getDistance d seconds)) ds

getDistances :: Set.Set Deer -> Int -> Set.Set (Int, Deer)
getDistances deer second = Set.map (\d -> (getDistance d second, d)) deer

updatePointsForStep :: Map.Map Deer Int -> Int -> Map.Map Deer Int
updatePointsForStep pointMap timeStep = let deerDist = getDistances (Map.keysSet pointMap) timeStep
                                            (maxDist, _) = Set.findMax deerDist
                                            (gotPoint, _) = Set.partition (\(dist, deer) -> dist >= maxDist) deerDist
                                        in Set.foldl' (\pmap (_, deer) -> Map.adjust (+1) deer pmap) pointMap gotPoint

deerMap :: Deer -> (Deer, Int)
deerMap d = (d, 0)

getWinner2 :: Set.Set Deer -> Int -> Int
getWinner2 ds seconds = let timeSteps = [1..seconds]
                            pointMap = Map.fromList $ List.map (\d -> (d, 0)) $ Set.toList ds
                            finalPointMap = List.foldl' (updatePointsForStep) pointMap timeSteps
                        in Map.foldl' (\maxdist d -> if d > maxdist then d else maxdist) 0 finalPointMap

