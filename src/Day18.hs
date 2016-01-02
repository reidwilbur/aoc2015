module Day18 (isValidNeighborIdx, numLitNeighbors, LightState(..), getNeighborIdxs, nextState, parseGrid, nextStateStuck) where
import Debug.Trace
import qualified Data.List as List
import qualified Data.Vector as Vect

data LightState = Lights { size :: Int, state :: Vect.Vector Bool } deriving (Show, Eq)

isValidNeighborIdx :: LightState -> Int -> Int -> Bool
isValidNeighborIdx lights idx nidx
  | nidx == idx = False
  | nidx < 0 = False
  | nidx >= (size lights)^2 = False
  | otherwise = let idxcol = idx `mod` size lights
                    nbrcol = nidx `mod` size lights
                    coldif = abs (idxcol - nbrcol)
                in if coldif > 1 then False else True

getNeighborIdxs :: LightState -> Int -> [Int]
getNeighborIdxs lights idx =
  let neighbors = [idx - 1 - size lights, idx - size lights, idx + 1 - size lights
                  ,idx - 1                                 , idx + 1
                  ,idx - 1 + size lights, idx + size lights, idx + 1 + size lights]
  in List.filter (isValidNeighborIdx lights idx) neighbors

numLitNeighbors :: LightState -> Int -> Int
numLitNeighbors lights idx =
  List.foldl' (\c i -> if (state lights) Vect.! i then c + 1 else c) 0 $ getNeighborIdxs lights idx

nextStateForIdx :: LightState -> Int -> Bool
nextStateForIdx lights idx = let nlit = numLitNeighbors lights idx
                             in if (state lights) Vect.! idx
                                  then (nlit == 2) || (nlit == 3)
                                  else 3 == nlit

nextStateForIdxStuck :: LightState -> Int -> Bool
nextStateForIdxStuck lights 0 = True
nextStateForIdxStuck lights idx | idx == (size lights) - 1 = True
nextStateForIdxStuck lights idx | idx == (size lights) * (size lights - 1) = True
nextStateForIdxStuck lights idx | idx == (size lights)^2 - 1 = True
nextStateForIdxStuck lights idx = let nlit = numLitNeighbors lights idx
                                  in if (state lights) Vect.! idx
                                       then (nlit == 2) || (nlit == 3)
                                       else 3 == nlit

nextState :: LightState -> LightState
nextState lights = Lights (size lights) (Vect.generate ((size lights)^2) (nextStateForIdx lights))

nextStateStuck :: LightState -> LightState
nextStateStuck lights = Lights (size lights) (Vect.generate ((size lights)^2) (nextStateForIdxStuck lights))

parseLine :: String -> Vect.Vector Bool
parseLine s = Vect.generate (List.length s) (\i -> (if (s List.!! i) == '#' then True else False))

parseGrid :: [String] -> LightState
parseGrid ss = Lights (List.length (ss List.!! 0)) (List.foldl' (\vect s -> vect Vect.++ parseLine s) Vect.empty ss)

