module Main where
import Test.Hspec
import Day20
import qualified Data.List as List
import qualified Data.Vector as Vect

spec :: Spec
spec = do
  describe "Day20" $ do
    context "genFactors" $ do
      it "should gen factors for 100" $ do
        genFactors 20 `shouldBe` [1,2,4,20,10,5]

    context "getPresentsIdxAtLeast getPresents" $ do
      it "should return (8, 150) for 130" $ do
        getPresentsIdxAtLeast getPresents 1 130 `shouldBe` (8, 150)

      it "should return (665280,_) for 29000000" $ do
        getPresentsIdxAtLeast getPresents 665000 29000000 `shouldBe` (665280,29260800)

    context "getPresentsIdxAtLeast getPresents2" $ do
      it "should return (6, 132) for 130" $ do
        getPresentsIdxAtLeast getPresents2 1 130 `shouldBe` (6, 132)

      it "should return (705600,29002446) for 29000000" $ do
        getPresentsIdxAtLeast getPresents2 705599 29000000 `shouldBe` (705600,29002446)

main :: IO ()
main = hspec spec

