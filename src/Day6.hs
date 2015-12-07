module Day6 (getLights, parseInstr, execInstr, Instr(TurnOn, Toggle, TurnOff), Addr, brightness) where
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type Addr = (Integer, Integer)

data Instr = TurnOn Addr Addr | Toggle Addr Addr | TurnOff Addr Addr deriving (Show, Eq)

getLights :: [String] -> Set.Set Addr
getLights is = List.foldl' execInstr Set.empty $ map parseInstr is

execInstr :: Set.Set Addr -> Instr -> Set.Set Addr
execInstr s (TurnOn  (r1,c1) (r2,c2)) = Set.union s $ Set.fromList [ (r,c) | r <- [r1..r2], c <- [c1..c2]]
execInstr s (TurnOff (r1,c1) (r2,c2)) = Set.difference s $ Set.fromList [ (r,c) | r <- [r1..r2], c <- [c1..c2]]
execInstr s (Toggle  (r1,c1) (r2,c2)) = let addrs = Set.fromList [ (r,c) | r <- [r1..r2], c <- [c1..c2]]
                                            litInBoth = Set.intersection s addrs
                                        in Set.union (Set.difference addrs litInBoth) (Set.difference s litInBoth)

brightness :: [String] -> Integer
brightness s = List.sum $ Map.elems $ List.foldl' execInstr2 Map.empty $ map parseInstr s

execInstr2 :: Map.Map Addr Integer -> Instr -> Map.Map Addr Integer
execInstr2 m (TurnOn  (r1,c1) (r2,c2)) = List.foldl' (\mm k -> Map.insertWith (+) k 1 mm) m [(r,c) | r <- [r1..r2], c <- [c1..c2]]
execInstr2 m (TurnOff (r1,c1) (r2,c2)) = List.foldl' (\mm k -> Map.insertWith (\n o -> if o > 0 then o - 1 else 0) k 0 mm) m [(r,c) | r <- [r1..r2], c <- [c1..c2]]
execInstr2 m (Toggle  (r1,c1) (r2,c2)) = List.foldl' (\mm k -> Map.insertWith (\n o -> o + 2) k 2 mm) m [(r,c) | r <- [r1..r2], c <- [c1..c2]]

parseAddr :: String -> Addr
parseAddr ss = let r = read (takeWhile (',' /=) ss) :: Integer
                   c = read (drop 1 (dropWhile (',' /=) ss)) :: Integer
               in (r,c)

parseInstr :: String -> Instr
parseInstr s
  | List.isPrefixOf "turn off " s = TurnOff (parseAddr $ words !! 2) (parseAddr $ words !! 4)
  | List.isPrefixOf "toggle "   s = Toggle  (parseAddr $ words !! 1) (parseAddr $ words !! 3)
  | List.isPrefixOf "turn on "  s = TurnOn  (parseAddr $ words !! 2) (parseAddr $ words !! 4)
  | otherwise = error ("Could not parse :" ++ s)
  where words = List.words s

