module Main where
import Test.Hspec
import Day17
import qualified Data.List as List
import qualified Data.Map as Map

within :: Double -> Double -> Double -> Bool
within d1 d2 err = if abs (d1 - d2) < err then True else False

spec :: Spec
spec = do
  describe "Day17" $ do
    context "getAllContainers" $ do
      it "should return correct container combinations for test input" $ do
        getAllContainers 25 day17TestInput `shouldSatisfy` (\ll -> 4 == List.length ll
                                                                   && (2 == List.length (List.findIndices (==[20,5]) ll))
                                                                   && (1 == List.length (List.findIndices (==[15,10]) ll))
                                                                   && (1 == List.length (List.findIndices (==[15,5,5]) ll)))

      it "should return 1638 container combinations for Day17Input" $ do
        List.length (getAllContainers 150 day17Input) `shouldBe` 1638

    context "getNumMinContainers" $ do
      it "should return List with length 3 for test input" $ do
        List.length (getNumMinContainers 25 day17TestInput) `shouldBe` 3

      it "should return List with length 17 for test input" $ do
        List.length (getNumMinContainers 150 day17Input) `shouldBe` 17

main :: IO ()
main = hspec spec

day17TestInput = [20,15,10,5,5]

day17Input = [
  43
  , 3
  , 4
  , 10
  , 21
  , 44
  , 4
  , 6
  , 47
  , 41
  , 34
  , 17
  , 17
  , 44
  , 36
  , 31
  , 46
  , 9
  , 27
  , 38]

