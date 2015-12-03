module Day3Test where
import qualified Distribution.TestSuite as Cabal
import qualified Distribution.TestSuite.HUnit as CabalHUnit
import Test.HUnit
import Day3

getNextAddressTests = [
  (getNextAddress '^' (0,0), (1,0))]

housesWDTests = [
  (housesWithDeliveries "^>v<", 4),
  (housesWithDeliveries "^v^v^v^v^v", 2),
  (housesWithDeliveries ">", 2),
  (housesWithDeliveries day3Input, 2081)]

housesWD2Tests = [
  (housesWithDeliveries2 "^v", 3),
  (housesWithDeliveries2 "^>v<", 3),
  (housesWithDeliveries2 "^v^v^v^v^v", 11),
  (housesWithDeliveries2 ">", 2),
  (housesWithDeliveries2 day3Input, 2341)]

genTestCases :: (Ord a, Show a) => String -> [(a, a)] -> [Test]
genTestCases s tcs = map (\(idx, (fn, exp)) -> TestLabel s (TestCase (assertEqual ("TestCase" ++ show idx) exp fn))) (zipWith (\i t -> (i, t)) [1..] tcs)

tests = TestList ((genTestCases "getNextAddressTests" getNextAddressTests)
    ++ (genTestCases "housesWD" housesWDTests)
    ++ (genTestCases "housesWD2" housesWD2Tests))

