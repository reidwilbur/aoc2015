module Day2Test where
import qualified Distribution.TestSuite as Cabal
import qualified Distribution.TestSuite.HUnit as CabalHUnit
import Test.HUnit
import Day2

getPaperAreaTests = [
  (getPaperArea (2,3,4), 58),
  (getPaperArea (1,1,10), 43)]

getTotalPaperAreaTests = [
  (getTotalPaperArea ["2x3x4"], 58),
  (getTotalPaperArea ["1x1x10"], 43),
  (getTotalPaperArea day2Input, 1598415)]

getDimsTests = [
  (getDims "1x23x123", (1,23,123))]

getRibbonLengthTests = [
  (getRibbonLength (2,3,4), 34),
  (getRibbonLength (1,1,10), 14)]

getTotalRibbonLengthTests = [
  (getTotalRibbonLength ["2x3x4"], 34),
  (getTotalRibbonLength ["1x1x10"], 14),
  (getTotalRibbonLength day2Input, 3812909)]

genTestCases :: (Ord a, Show a) => String -> [(a, a)] -> [Test]
genTestCases s tcs = map (\(idx, (fn, exp)) -> TestLabel s (TestCase (assertEqual ("TestCase" ++ show idx) exp fn))) (zipWith (\i t -> (i, t)) [1..] tcs)

tests = TestList (
    (genTestCases "getPaperAreaTests" getPaperAreaTests) 
    ++ (genTestCases "getTotalPaperAreaTests" getTotalPaperAreaTests) 
    ++ (genTestCases "getDimsTests" getDimsTests) 
    ++ (genTestCases "getRibbonLengthTests" getRibbonLengthTests) 
    ++ (genTestCases "getTotalRibbonLengthTests" getTotalRibbonLengthTests))

