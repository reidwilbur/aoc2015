module Main where
import Test.Hspec
import Day4

spec :: Spec
spec = do
  describe "Day4" $ do
    context "getHash1" $ do
      it "should return 11 for key day4Input" $ do
        let (idx, _) = getHash1 day4Input in idx `shouldBe` 11

-- These take a while to run
--    context "getHash5" $ do
--      it "should return 609043 for key abcdef" $ do
--        let (idx, _) = getHash5 "abcdef" in idx `shouldBe` 609043

--      it "should return 1048970 for key pqrstuv" $ do
--        let (idx, _) = getHash "pqrstuv" in idx `shouldBe` 1048970

--      it "should return 117946 for key day4Input" $ do
--        let (idx, _) = getHash5 day4Input in idx `shouldBe` 117946

--    context "getHash6" $ do
--      it "should return 3938038 for day4Input" $ do
--        let (idx, _) = getHash6 day4Input in idx `shouldBe` 3938038

main :: IO ()
main = hspec spec

day4Input = "ckczppom"
