module Main where
import Test.Hspec
import Day14
import qualified Data.List as List
import qualified Data.Set as Set

comet = Deer "Comet" 14 10 127
dancer = Deer "Dancer" 16 11 162

spec :: Spec
spec = do
  describe "Day14" $ do
    context "parseDeer" $ do
      it "should parse correctly" $ do
        let s = "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds." 
        parseDeer s `shouldBe` (Deer "Rudolph" 22 8 165)

    context "getDistance" $ do
      it "should return 14 for Comet for 1s" $ do
        getDistance comet 1 `shouldBe` 14

      it "should return 16 for Dancer for 1s" $ do
        getDistance dancer 1 `shouldBe` 16

      it "should return 140 for Comet for 10s" $ do
        getDistance comet 10 `shouldBe` 140

      it "should return 160 for Dancer for 10s" $ do
        getDistance dancer 10 `shouldBe` 160

      it "should return 140 for Comet for 11s" $ do
        getDistance comet 11 `shouldBe` 140

      it "should return 176 for Dancer for 11s" $ do
        getDistance dancer 11 `shouldBe` 176

      it "should return 140 for Comet for 12s" $ do
        getDistance comet 12 `shouldBe` 140

      it "should return 176 for Dancer for 12s" $ do
        getDistance dancer 12 `shouldBe` 176

      it "should return 1120 for Comet for 1000s" $ do
        getDistance comet 1000 `shouldBe` 1120

      it "should return 1056 for Dancer for 1000s" $ do
        getDistance dancer 1000 `shouldBe` 1056

    context "getWinner" $ do
      it "should return 2696 for day14Input 2503s" $ do
        let deer = List.map parseDeer day14Input
        getWinner deer 2503 `shouldBe` ("Cupid", 2696)

    context "getWinner2" $ do
      it "should return 1 for day14TestInput 1s" $ do
        let deer = Set.fromList [Deer "Comet" 14 10 127, Deer "Dancer" 16 11 162]
        getWinner2 deer 1 `shouldBe` 1

      it "should return 139 for day14TestInput 140s" $ do
        let deer = Set.fromList [Deer "Comet" 14 10 127, Deer "Dancer" 16 11 162]
        getWinner2 deer 140 `shouldBe` 139

      it "should return 689 for day14TestInput 1000s" $ do
        let deer = Set.fromList [Deer "Comet" 14 10 127, Deer "Dancer" 16 11 162]
        getWinner2 deer 1000 `shouldBe` 689

      it "should return 1084 for day14Input 2503s" $ do
        let deer = Set.fromList $ List.map parseDeer day14Input
        getWinner2 deer 2503 `shouldBe` 1084

main :: IO ()
main = hspec spec

day14Input = [
  "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.",
  "Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.",
  "Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.",
  "Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.",
  "Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.",
  "Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.",
  "Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.",
  "Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.",
  "Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds."]
