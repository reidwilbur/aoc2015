module Day11 (hasIncreasingTriple, hasTwoPairs, base26ToInt, intToBase26, hasIllegalChars, isValidPasswd, getNextPasswd) where
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Char as Char

hasIncreasingTriple :: String -> Bool
hasIncreasingTriple (a:b:c:ss) = let oa = Char.ord a 
                                     ob = Char.ord b - oa
                                     oc = Char.ord c - oa
                                 in case (ob,oc) of
                                      (1,2) -> True
                                      _ -> hasIncreasingTriple (b:c:ss)
hasIncreasingTriple _ = False

pairSet = Set.fromList[
  "aa","bb","cc","dd","ee","ff"
  ,"gg","hh","ii","jj","kk","ll"
  ,"mm","nn","oo","pp","qq","rr"
  ,"ss","tt","uu","vv","ww","xx"
  ,"yy","zz"]

hasTwoPairsInSet :: Set.Set String -> String -> Bool
hasTwoPairsInSet ps (c1:c2:ss) = let p = [c1,c2]
                                     nps = Set.delete p ps
                                 in if (Set.size pairSet - Set.size nps) == 2 
                                    then True
                                    else hasTwoPairsInSet nps (c2:ss)
hasTwoPairsInSet _ _ = False

hasTwoPairs = hasTwoPairsInSet pairSet

hasIllegalChars :: String -> Bool
hasIllegalChars ss = let ics = Set.fromList['i','o','l']
                     in List.foldr (\c tf -> tf || (Set.member c ics)) False ss

base26ChrToInt :: Char -> Int
base26ChrToInt c = Char.ord c - Char.ord 'a'

base26ToInt :: String -> Int
base26ToInt ss = let zipped = List.zipWith (\i s -> (i,s)) [0..] $ List.reverse ss
                 in List.foldr (\(exp, c) i -> ((base26ChrToInt c) * (26 ^ exp)) + i) 0 zipped

appendBase26Chr :: (String,Int) -> Int -> (String, Int)
appendBase26Chr (b26s, i) pwr = let exp = 26 ^ pwr
                                    c = Char.chr $ (i `div` exp) + Char.ord 'a'
                                    r = i `mod` exp
                                in (c:b26s, r)

intToBase26 :: Int -> String
intToBase26 i = let maxexp = if i == 0 then 0 else floor $ logBase 26 (fromIntegral i)
                    exps = List.reverse [0..maxexp]
                    (s, _) = List.foldl' appendBase26Chr ("", i) exps
                in List.reverse s

isValidPasswd :: String -> Bool
isValidPasswd s = hasTwoPairs s && hasIncreasingTriple s && (not $ hasIllegalChars s)

padPasswd :: String -> String
padPasswd psswd | List.length psswd >= 8 = psswd
                | otherwise = let padlen = 8 - List.length psswd
                              in (List.replicate padlen 'a') ++ psswd

getNextPasswd :: String -> String
getNextPasswd pswd = let is = List.tail [(base26ToInt pswd)..]
                         pswds = List.map intToBase26 is
                         valid = List.filter isValidPasswd pswds
                     in padPasswd $ List.head valid

