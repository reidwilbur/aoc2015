module Day5 (isNiceString, isNiceString2, countNiceStrings, countNiceStrings2) where
import Debug.Trace
import Text.Regex.PCRE

isNiceString :: String -> Bool
isNiceString s = let has3Vowels = (s =~ "[aeiou].*[aeiou].*[aeiou]")
                     hasDoubleChar = (s =~ "(.)\\1{1,}")
                     hasBadPairs = (s =~ "ab|cd|pq|xy")
                 in has3Vowels && hasDoubleChar && not hasBadPairs

isNiceString2 :: String -> Bool
isNiceString2 s = let hasDoublePair = (s =~ "(..).*\\1")
                      hasSurroundChar = (s =~ "(.).\\1")
                  in hasDoublePair && hasSurroundChar

countNiceStrings = countStrings isNiceString

countNiceStrings2 = countStrings isNiceString2

countStrings :: (String -> Bool) -> [String] -> Int
countStrings isNice ss = length $ filter isNice ss

