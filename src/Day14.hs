module Day14 (Deer(Deer), parseDeer, getDistance, getWinner) where
import Debug.Trace
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Function as Fn

data Deer = Deer {name :: String
                  , speed :: Int
                  , runtime :: Int
                  , resttime :: Int} 
                 deriving (Show, Eq)

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

