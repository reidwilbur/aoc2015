module Main where
import Test.Hspec
import Day10
import qualified Data.List as List

spec :: Spec
spec = do
  describe "Day10" $ do
    context "getNextSeq" $ do
      it "should return 11 for 1" $ do
        getNextSeq "1" `shouldBe` "11"

      it "should return 21 for 11" $ do
        getNextSeq "11" `shouldBe` "21"

      it "should return 1211 for 21" $ do
        getNextSeq "21" `shouldBe` "1211"

      it "should return 312211 for 111221" $ do
        getNextSeq "111221" `shouldBe` "312211"

      it "should return string with len 252594 after 40 iterations on 1113222113" $ do
        let seq = List.foldl' (\s c -> getNextSeq s) "1113222113" [0..39] in
          List.length seq `shouldBe` 252594

      it "should return string with len 3579328 after 50 iterations on 1113222113" $ do
        let seq = List.foldl' (\s c -> getNextSeq s) "1113222113" [0..49] in
          List.length seq `shouldBe` 3579328 

main :: IO ()
main = hspec spec

