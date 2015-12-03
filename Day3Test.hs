module Day3Test where
import Test.HUnit
import Day3

getNextAddressTests = [
  (getNextAddress '^' (0,0), (1,0))]

housesWDTests = [
  (housesWithDeliveries "^>v<", 4),
  (housesWithDeliveries "^v^v^v^v^v", 2),
  (housesWithDeliveries ">", 2),
  (housesWithDeliveries day3Input, 2081)]

genTestCases :: (Ord a, Show a) => String -> [(a, a)] -> [Test]
genTestCases s tcs = map (\(idx, (fn, exp)) -> TestLabel s (TestCase (assertEqual ("TestCase" ++ show idx) exp fn))) (zipWith (\i t -> (i, t)) [1..] tcs)

tests = TestList ((genTestCases "getNextAddressTests" getNextAddressTests)
    ++ (genTestCases "housesWMD" housesWDTests))

