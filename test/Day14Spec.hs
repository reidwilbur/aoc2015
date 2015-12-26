module Main where
import Test.Hspec
import Day14
import qualified Data.List as List

spec :: Spec
spec = do
  describe "Day14" $ do
    context "asdf" $ do
      it "sadf" $ do
        True `shouldBe` True


main :: IO ()
main = hspec spec

