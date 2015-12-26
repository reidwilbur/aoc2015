module Main where
import Test.Hspec
import Day8
import qualified Data.List as List

getTestInput :: String -> IO [String]
getTestInput s = do
                   contents <- readFile $ "test/" ++ s ++ ".txt"
                   return $ List.lines contents

spec :: Spec
spec = do
  describe "Day8" $ do
    context "literalLen" $ do
      it "should return 2 for empty String" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (testLines !! 0) `shouldBe` 2

      it "should return 5 for 'abc'" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (testLines !! 1) `shouldBe` 5
    
      it "should return 10 for aaa\\\"aaa" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (testLines !! 2) `shouldBe` 10

      it "should return 6 for \\x27" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (testLines !! 3) `shouldBe` 6

    context "codeLen" $ do
      it "should return 0 for empty String" $ do
        testLines <- getTestInput "Day8TestInput1"
        codeLen (testLines !! 0) `shouldBe` 0

      it "should return 3 for 'abc'" $ do
        testLines <- getTestInput "Day8TestInput1"
        codeLen (testLines !! 1) `shouldBe` 3
    
      it "should return 7 for aaa\\\"aaa" $ do
        testLines <- getTestInput "Day8TestInput1"
        codeLen (testLines !! 2) `shouldBe` 7

      it "should return 1 for \\x27" $ do
        testLines <- getTestInput "Day8TestInput1"
        codeLen (testLines !! 3) `shouldBe` 1

    context "literalMinusCodeLen" $ do
      it "should be 12 for test input" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalMinusCodeLen testLines `shouldBe` 12

      it "should be 1333 for test part 1 input" $ do
        testLines <- getTestInput "Day8Part1TestInput"
        literalMinusCodeLen testLines `shouldBe` 1333

    context "escapedCodeString" $ do
      it "should have literalLen 6 for test input 0" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (escapedCodeString (testLines !! 0)) `shouldBe` 6

      it "should have literalLen 9 for test input 1" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (escapedCodeString (testLines !! 1)) `shouldBe` 9

      it "should have literalLen 16 for test input 2" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (escapedCodeString (testLines !! 2)) `shouldBe` 16

      it "should have literalLen 11 for test input 2" $ do
        testLines <- getTestInput "Day8TestInput1"
        literalLen (escapedCodeString (testLines !! 3)) `shouldBe` 11

    context "escapedliteralMinusLiteralLen" $ do
      it "should be 19 for test input" $ do
        testLines <- getTestInput "Day8TestInput1"
        escapedLiteralMinusLiteralLen testLines `shouldBe` 19

      it "should be 2046 for test part 1 input" $ do
        testLines <- getTestInput "Day8Part1TestInput"
        escapedLiteralMinusLiteralLen testLines `shouldBe` 2046

main :: IO ()
main = hspec spec

