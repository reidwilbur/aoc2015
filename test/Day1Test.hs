module Day1Test (tests) where

import Test.HUnit
import Day1

testCases = [
  (getFloor "(())", 0),
  (getFloor "()()", 0),
  (getFloor "(((", 3),
  (getFloor "(()(()(", 3),
  (getFloor "))(((((", 3),
  (getFloor "())", -1),
  (getFloor "))(", -1),
  (getFloor ")))", -3),
  (getFloor ")())())", -3),
  (getFloor day1Input, 74),
  (firstBasement ")", 1),
  (firstBasement "()())", 5),
  (firstBasement day1Input, 1795)
  ]

tests = TestList (map (\t@(fn, exp) -> TestLabel (show t) (TestCase (assertEqual (show t) exp fn))) testCases)
