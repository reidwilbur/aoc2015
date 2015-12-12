module Day8 (literalLen, codeLen, literalMinusCodeLen, escapedCodeString, escapedLiteralMinusLiteralLen) where
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map.Strict as Map

literalLen :: String -> Int
literalLen = List.length

collapseCount :: String -> Int
collapseCount s = case s of
                   ('\\':'"':s)     -> 1 + (collapseCount s)
                   ('\\':'\\':s)    -> 1 + (collapseCount s)
                   ('\\':'x':_:_:s) -> 1 + (collapseCount s)
                   (_:s)            -> 1 + (collapseCount s)
                   []               -> 0


codeLen :: String -> Int
codeLen s = collapseCount $ List.init $ List.tail s

literalMinusCodeLen :: [String] -> Int
literalMinusCodeLen [] = 0
literalMinusCodeLen (s:ss) = (literalLen s) - (codeLen s) + (literalMinusCodeLen ss)

escapedLiteralMinusLiteralLen :: [String] -> Int
escapedLiteralMinusLiteralLen ss = let escapedLiteralLen = List.foldr (+) 0 $ List.map literalLen $ List.map escapedCodeString ss
                                       literallen = List.foldr (+) 0 $ List.map literalLen ss
                                   in escapedLiteralLen - literallen

escapeChar :: Char -> String
escapeChar '\\' = ['\\','\\'] 
escapeChar '"' = ['\\','"']
escapeChar c = [c]

escapedCodeString :: String -> String
escapedCodeString s = ['"','\\','"'] 
                      ++ (foldr (\c str -> (escapeChar c) ++ str) "" $ List.init $ List.tail s) 
                      ++ ['\\','"','"']

