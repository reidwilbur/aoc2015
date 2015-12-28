module Day16 (parseSue, Sue(Sue), similarity, getBestMatch) where
import Debug.Trace
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Sue = Sue { name :: String
                 , attrs :: Map.Map String Int } deriving (Show, Eq, Ord)

similarity :: Map.Map String Int -> Map.Map String Int -> Double
similarity m1 m2 = let commonKeys = Set.intersection (Map.keysSet m2) (Map.keysSet m1)
                       dotprod = Set.foldl' (\s k -> s + ((m1 Map.! k) * (m2 Map.! k))) 0 commonKeys
                       m1mag = sqrt $ fromIntegral $ Set.foldl' (\s k -> s + (m1 Map.! k)^2) 0 commonKeys
                       m2mag = sqrt $ fromIntegral $ Set.foldl' (\s k -> s + (m2 Map.! k)^2) 0 commonKeys
                   in (fromIntegral dotprod) / (m1mag * m2mag)

getSimilarities :: Map.Map String Int -> [Sue] -> [(Double, Sue)]
getSimilarities vect sues = List.map (\s -> (similarity vect (attrs s), s)) sues

getBestMatch :: Map.Map String Int -> [Sue] -> (Double, Sue)
getBestMatch vect sues = let simPairs = List.sort $ getSimilarities vect sues
                         in List.last simPairs

split :: Char -> String -> [String]
split _ "" = []
split c ss = (List.takeWhile (/=c) ss) : split c (List.dropWhile (==c) $ List.dropWhile (/=c) ss)

getParm :: String -> (String, Int)
getParm ss = let parts = split ':' ss
             in case parts of
                  (n:v:_) -> (List.dropWhile (==' ') n, read v)
                  _ -> error ("Can't parse parm" ++ ss)

addParm :: Map.Map String Int -> (String, Int) -> Map.Map String Int
addParm map (k, v) = Map.insert k v map

parseSue :: String -> Sue
parseSue ss = let name = List.takeWhile (/=':') ss
                  rawParms = split ',' $ List.dropWhile (==':') $ List.dropWhile (/=':') ss
                  parmMap = List.foldl' (\map pstr -> addParm map $ getParm pstr) Map.empty rawParms
              in Sue name parmMap 

