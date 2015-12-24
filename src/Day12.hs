module Day12 (parseJson, sumJsonInts, sumJsonIntsNoRedOs, JValue(JString, JDouble, JInt, JObject, JArray)) where
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Char as Char
import Text.Read (readMaybe)

data JValue = JString String 
            | JDouble Double 
            | JInt Int 
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getWord :: ([String], String, [Char]) -> Char -> ([String], String, [Char])
getWord (words, word, stack) c = case (c, stack) of
                                   (',', [])    -> ((List.reverse word):words, "", stack)
                                   ('[', _)     -> (words, c:word, c:stack)
                                   ('{', _)     -> (words, c:word, c:stack)
                                   (']', '[':s) -> (words, c:word, s)
                                   ('}', '{':s) -> (words, c:word, s)
                                   (_, _)       -> (words, c:word, stack)

-- this does not handle commas embedded in strings
-- it is a hack and will break for general input
jsonWords :: String -> [String]
jsonWords "" = []
jsonWords ss = let (words, rest, _) = List.foldl' getWord ([], "", []) ss
               in case rest of
                    "" -> List.reverse words
                    _ -> List.reverse $ (List.reverse rest):words

stripQuotes :: String -> String
stripQuotes "" = ""
stripQuotes ss = let h = List.head ss
                     l = List.last ss
                 in case (h,l) of
                      ('"','"') -> List.init $ List.tail ss
                      _ -> ss

parseObjectField :: String -> (String, JValue)
parseObjectField ss = let fname = List.takeWhile (/=':') ss
                          value = List.tail $ List.dropWhile (/=':') ss
                      in (stripQuotes fname, parseJson value)

parseObject :: String -> [(String, JValue)]
parseObject = List.foldr (\word fields -> parseObjectField word : fields) [] . jsonWords

parseArray :: String -> [JValue]
parseArray = List.foldr (\word elems -> parseJson word : elems) [] . jsonWords

parseNumber :: String -> JValue
parseNumber ss = let ival = readMaybe ss :: Maybe Int
                 in case ival of
                      Just i -> JInt i
                      _ -> let dval = readMaybe ss :: Maybe Double
                           in case dval of
                                Just d -> JDouble d
                                _ -> error ("Can't parse number from " ++ ss)

parseJson :: String -> JValue
parseJson ss = let hl = (List.head ss, List.last ss, List.init $ List.tail ss) 
               in case hl of
                    ('{','}', content) -> JObject $ parseObject content
                    ('[',']', content) -> JArray $ parseArray content
                    ('"','"', content) -> JString content
                    _ -> parseNumber ss

sumJsonInts :: JValue -> Int
sumJsonInts (JInt i) = i
sumJsonInts (JObject fields) = List.foldl' (\sum (fn, jv) -> sumJsonInts jv + sum) 0 fields
sumJsonInts (JArray vals) = List.foldl' (\sum jv -> sumJsonInts jv + sum) 0 vals
sumJsonInts _ = 0

sumJsonIntsNoRedOs :: JValue -> Int
sumJsonIntsNoRedOs (JInt i) = i
sumJsonIntsNoRedOs (JObject fields) = let fvals = List.map (\(fn, fv) -> fv) fields
                                          hasRed = List.foldl' (\hr fv -> case fv of
                                                                            JString "red" -> True
                                                                            _ -> hr) False fvals
                                      in if hasRed then 0
                                         else List.foldl' (\sum (fn, jv) -> sumJsonIntsNoRedOs jv + sum) 0 fields
sumJsonIntsNoRedOs (JArray vals) = List.foldl' (\sum jv -> sumJsonIntsNoRedOs jv + sum) 0 vals
sumJsonIntsNoRedOs _ = 0

