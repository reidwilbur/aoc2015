module Day11Spec where
import Test.Hspec
import Day11
import qualified Data.List as List

spec :: Spec
spec = do
  describe "Day11" $ do
    context "hasIncreasingTriple" $ do
      it "returns True for hijklmmn" $ do
        hasIncreasingTriple "hijklmmn" `shouldBe` True

      it "returns False for abbceffg" $ do
        hasIncreasingTriple "abbceffg" `shouldBe` False

      it "returns False for abbcegjk" $ do
        hasIncreasingTriple "abbcegjk" `shouldBe` False

      it "returns False for ghjaabcc" $ do
        hasIncreasingTriple "ghjaabcc" `shouldBe` True

    context "hasTwoPairs" $ do
      it "returns True for abbceffg" $ do
        hasTwoPairs "abbceffg" `shouldBe` True

      it "returns True for abbcegjk" $ do
        hasTwoPairs "abbcegjk" `shouldBe` False

      it "returns True for ghjaabcc" $ do
        hasTwoPairs "ghjaabcc" `shouldBe` True

    context "hasIllegalChars" $ do
      it "returns True for hijklmmn" $ do
        hasIllegalChars "hijklmmn" `shouldBe` True

      it "returns False for abbceffg" $ do
        hasIllegalChars "abbceffg" `shouldBe` False

      it "returns False for ghjaabcc" $ do
        hasIllegalChars "ghjaabcc" `shouldBe` False

    context "base26ToInt" $ do
      it "returns 0 for a" $ do
        base26ToInt "a" `shouldBe` 0

      it "returns 1 for b" $ do
        base26ToInt "b" `shouldBe` 1

      it "returns 25 for z" $ do
        base26ToInt "z" `shouldBe` 25

      it "returns 26 for ba" $ do
        base26ToInt "ba" `shouldBe` 26

      it "returns 26^2 for baa" $ do
        base26ToInt "baa" `shouldBe` (26 ^ 2)

    context "intToBase26" $ do
      it "returns a for 0" $ do
        intToBase26 0 `shouldBe` "a"

      it "returns z for 25" $ do
        intToBase26 25 `shouldBe` "z"

      it "returns ba for 26" $ do
        intToBase26 26 `shouldBe` "ba"

      it "reutrns baa for 26^2" $ do
        intToBase26 (26^2) `shouldBe` "baa"

    context "isValidPasswd" $ do
      it "returns False for hijklmmn" $ do
        isValidPasswd "hijklmmn" `shouldBe` False

      it "returns False for abbceffg" $ do
        isValidPasswd "abbceffg" `shouldBe` False

      it "returns False for abbcegjk" $ do
        isValidPasswd "abbcegjk" `shouldBe` False

      it "returns False for abcdffaa" $ do
        isValidPasswd "abcdffaa" `shouldBe` True

      it "returns False for ghjaabcc" $ do
        isValidPasswd "ghjaabcc" `shouldBe` True

    context "getNextPasswd" $ do
      it "returns abcdffaa for abcdefgh" $ do
        getNextPasswd "abcdefgh" `shouldBe` "abcdffaa"

-- this takes a while to run
--      it "returns ghjaabcc for ghijklmn" $ do
--        getNextPasswd "ghijklmn" `shouldBe` "ghjaabcc"

      it "returns vzbxxyzz for vzbxkghb" $ do
        getNextPasswd "vzbxkghb" `shouldBe` "vzbxxyzz"

      it "returns vzcaabcc for vzbxxyzz" $ do
        getNextPasswd "vzbxxyzz" `shouldBe` "vzcaabcc"

main :: IO ()
main = hspec spec

