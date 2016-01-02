module Main where
import Test.Hspec
import Day13
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Day13" $ do
    context "parseInput" $ do
      it "should parse input into map" $ do
        parseInput (List.take 3 day13TestInput) `shouldBe`
          Map.fromList [("Alice", Map.fromList [("Bob", 54), ("Carol", -79), ("David", -2)])]

    context "getHappiness" $ do
      it "should return 330 for test input config" $ do
        let costmap = parseInput day13TestInput
        let config = ["David", "Alice", "Bob", "Carol"]
        getHappiness costmap config `shouldBe` 330

    context "getMaxHappiness" $ do
      it "should return 330 for test input" $ do
        let costmap = parseInput day13TestInput
        getMaxHappiness costmap `shouldSatisfy` (\c -> fst c == 330)

      it "should return 709 for day 13 input" $ do
        let costmap = parseInput day13Input
        getMaxHappiness costmap `shouldSatisfy` (\c -> fst c == 709)

      it "should return 688 with me in map" $ do
        let costmap = parseInput day13Input2
        getMaxHappiness costmap `shouldSatisfy` (\c -> fst c == 668)

main :: IO ()
main = hspec spec

day13TestInput = [
  "Alice would gain 54 happiness units by sitting next to Bob.",
  "Alice would lose 79 happiness units by sitting next to Carol.",
  "Alice would lose 2 happiness units by sitting next to David.",
  "Bob would gain 83 happiness units by sitting next to Alice.",
  "Bob would lose 7 happiness units by sitting next to Carol.",
  "Bob would lose 63 happiness units by sitting next to David.",
  "Carol would lose 62 happiness units by sitting next to Alice.",
  "Carol would gain 60 happiness units by sitting next to Bob.",
  "Carol would gain 55 happiness units by sitting next to David.",
  "David would gain 46 happiness units by sitting next to Alice.",
  "David would lose 7 happiness units by sitting next to Bob.",
  "David would gain 41 happiness units by sitting next to Carol."]

day13Input = [
  "Alice would gain 54 happiness units by sitting next to Bob.",
  "Alice would lose 81 happiness units by sitting next to Carol.",
  "Alice would lose 42 happiness units by sitting next to David.",
  "Alice would gain 89 happiness units by sitting next to Eric.",
  "Alice would lose 89 happiness units by sitting next to Frank.",
  "Alice would gain 97 happiness units by sitting next to George.",
  "Alice would lose 94 happiness units by sitting next to Mallory.",
  "Bob would gain 3 happiness units by sitting next to Alice.",
  "Bob would lose 70 happiness units by sitting next to Carol.",
  "Bob would lose 31 happiness units by sitting next to David.",
  "Bob would gain 72 happiness units by sitting next to Eric.",
  "Bob would lose 25 happiness units by sitting next to Frank.",
  "Bob would lose 95 happiness units by sitting next to George.",
  "Bob would gain 11 happiness units by sitting next to Mallory.",
  "Carol would lose 83 happiness units by sitting next to Alice.",
  "Carol would gain 8 happiness units by sitting next to Bob.",
  "Carol would gain 35 happiness units by sitting next to David.",
  "Carol would gain 10 happiness units by sitting next to Eric.",
  "Carol would gain 61 happiness units by sitting next to Frank.",
  "Carol would gain 10 happiness units by sitting next to George.",
  "Carol would gain 29 happiness units by sitting next to Mallory.",
  "David would gain 67 happiness units by sitting next to Alice.",
  "David would gain 25 happiness units by sitting next to Bob.",
  "David would gain 48 happiness units by sitting next to Carol.",
  "David would lose 65 happiness units by sitting next to Eric.",
  "David would gain 8 happiness units by sitting next to Frank.",
  "David would gain 84 happiness units by sitting next to George.",
  "David would gain 9 happiness units by sitting next to Mallory.",
  "Eric would lose 51 happiness units by sitting next to Alice.",
  "Eric would lose 39 happiness units by sitting next to Bob.",
  "Eric would gain 84 happiness units by sitting next to Carol.",
  "Eric would lose 98 happiness units by sitting next to David.",
  "Eric would lose 20 happiness units by sitting next to Frank.",
  "Eric would lose 6 happiness units by sitting next to George.",
  "Eric would gain 60 happiness units by sitting next to Mallory.",
  "Frank would gain 51 happiness units by sitting next to Alice.",
  "Frank would gain 79 happiness units by sitting next to Bob.",
  "Frank would gain 88 happiness units by sitting next to Carol.",
  "Frank would gain 33 happiness units by sitting next to David.",
  "Frank would gain 43 happiness units by sitting next to Eric.",
  "Frank would gain 77 happiness units by sitting next to George.",
  "Frank would lose 3 happiness units by sitting next to Mallory.",
  "George would lose 14 happiness units by sitting next to Alice.",
  "George would lose 12 happiness units by sitting next to Bob.",
  "George would lose 52 happiness units by sitting next to Carol.",
  "George would gain 14 happiness units by sitting next to David.",
  "George would lose 62 happiness units by sitting next to Eric.",
  "George would lose 18 happiness units by sitting next to Frank.",
  "George would lose 17 happiness units by sitting next to Mallory.",
  "Mallory would lose 36 happiness units by sitting next to Alice.",
  "Mallory would gain 76 happiness units by sitting next to Bob.",
  "Mallory would lose 34 happiness units by sitting next to Carol.",
  "Mallory would gain 37 happiness units by sitting next to David.",
  "Mallory would gain 40 happiness units by sitting next to Eric.",
  "Mallory would gain 18 happiness units by sitting next to Frank.",
  "Mallory would gain 7 happiness units by sitting next to George."]

day13Input2 = [
  "Alice would gain 54 happiness units by sitting next to Bob.",
  "Alice would lose 81 happiness units by sitting next to Carol.",
  "Alice would lose 42 happiness units by sitting next to David.",
  "Alice would gain 89 happiness units by sitting next to Eric.",
  "Alice would lose 89 happiness units by sitting next to Frank.",
  "Alice would gain 97 happiness units by sitting next to George.",
  "Alice would lose 94 happiness units by sitting next to Mallory.",
  "Alice would gain 0 happiness units by sitting next to Reid.",
  "Bob would gain 3 happiness units by sitting next to Alice.",
  "Bob would lose 70 happiness units by sitting next to Carol.",
  "Bob would lose 31 happiness units by sitting next to David.",
  "Bob would gain 72 happiness units by sitting next to Eric.",
  "Bob would lose 25 happiness units by sitting next to Frank.",
  "Bob would lose 95 happiness units by sitting next to George.",
  "Bob would gain 11 happiness units by sitting next to Mallory.",
  "Bob would gain 0 happiness units by sitting next to Reid.",
  "Carol would lose 83 happiness units by sitting next to Alice.",
  "Carol would gain 8 happiness units by sitting next to Bob.",
  "Carol would gain 35 happiness units by sitting next to David.",
  "Carol would gain 10 happiness units by sitting next to Eric.",
  "Carol would gain 61 happiness units by sitting next to Frank.",
  "Carol would gain 10 happiness units by sitting next to George.",
  "Carol would gain 29 happiness units by sitting next to Mallory.",
  "Carol would gain 0 happiness units by sitting next to Reid.",
  "David would gain 67 happiness units by sitting next to Alice.",
  "David would gain 25 happiness units by sitting next to Bob.",
  "David would gain 48 happiness units by sitting next to Carol.",
  "David would lose 65 happiness units by sitting next to Eric.",
  "David would gain 8 happiness units by sitting next to Frank.",
  "David would gain 84 happiness units by sitting next to George.",
  "David would gain 9 happiness units by sitting next to Mallory.",
  "David would gain 0 happiness units by sitting next to Reid.",
  "Eric would lose 51 happiness units by sitting next to Alice.",
  "Eric would lose 39 happiness units by sitting next to Bob.",
  "Eric would gain 84 happiness units by sitting next to Carol.",
  "Eric would lose 98 happiness units by sitting next to David.",
  "Eric would lose 20 happiness units by sitting next to Frank.",
  "Eric would lose 6 happiness units by sitting next to George.",
  "Eric would gain 60 happiness units by sitting next to Mallory.",
  "Eric would gain 0 happiness units by sitting next to Reid.",
  "Frank would gain 51 happiness units by sitting next to Alice.",
  "Frank would gain 79 happiness units by sitting next to Bob.",
  "Frank would gain 88 happiness units by sitting next to Carol.",
  "Frank would gain 33 happiness units by sitting next to David.",
  "Frank would gain 43 happiness units by sitting next to Eric.",
  "Frank would gain 77 happiness units by sitting next to George.",
  "Frank would lose 3 happiness units by sitting next to Mallory.",
  "Frank would gain 0 happiness units by sitting next to Reid.",
  "George would lose 14 happiness units by sitting next to Alice.",
  "George would lose 12 happiness units by sitting next to Bob.",
  "George would lose 52 happiness units by sitting next to Carol.",
  "George would gain 14 happiness units by sitting next to David.",
  "George would lose 62 happiness units by sitting next to Eric.",
  "George would lose 18 happiness units by sitting next to Frank.",
  "George would lose 17 happiness units by sitting next to Mallory.",
  "George would gain 0 happiness units by sitting next to Reid.",
  "Mallory would lose 36 happiness units by sitting next to Alice.",
  "Mallory would gain 76 happiness units by sitting next to Bob.",
  "Mallory would lose 34 happiness units by sitting next to Carol.",
  "Mallory would gain 37 happiness units by sitting next to David.",
  "Mallory would gain 40 happiness units by sitting next to Eric.",
  "Mallory would gain 18 happiness units by sitting next to Frank.",
  "Mallory would gain 7 happiness units by sitting next to George.",
  "Mallory would gain 0 happiness units by sitting next to Reid.",
  "Reid would gain 0 happiness units by sitting next to Alice.",
  "Reid would gain 0 happiness units by sitting next to Bob.",
  "Reid would gain 0 happiness units by sitting next to Carol.",
  "Reid would gain 0 happiness units by sitting next to David.",
  "Reid would gain 0 happiness units by sitting next to Eric.",
  "Reid would gain 0 happiness units by sitting next to Frank.",
  "Reid would gain 0 happiness units by sitting next to George.",
  "Reid would gain 0 happiness units by sitting next to Mallory."]

