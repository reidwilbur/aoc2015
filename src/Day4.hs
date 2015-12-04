module Day4 (getHash1, getHash5, getHash6) where
import qualified Data.Hash.MD5 as MD5
import Debug.Trace
import qualified Data.List as List

hash :: String -> String
hash s = MD5.md5s $ MD5.Str s

getHash1 = getHash "0"

getHash5 = getHash "00000"

getHash6 = getHash "000000"

getHash :: String -> String -> (Integer, String)
getHash prefix secret = let idxHashes = map (\i -> (i, hash $ secret ++ show i)) [0..]
                            filtered = filter (\(i, hash) -> List.isPrefixOf prefix hash) idxHashes
                        in head filtered

