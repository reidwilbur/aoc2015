module Main where
import Test.Hspec
import Day12
import qualified Data.List as List
import qualified Data.Text as Text

getTestInput :: String -> IO String
getTestInput s = do
                   contents <- readFile $ "test/" ++ s ++ ".txt"
                   return $ Text.unpack $ Text.strip $ Text.pack contents

spec :: Spec
spec = do
  describe "Day12" $ do
    context "parseJson" $ do
      it "should return JArray for [1,2,3]" $ do
        parseJson "[1,2,3]" `shouldBe` JArray [JInt 1, JInt 2, JInt 3]

      it "should return JObject for {a:2,b:4}" $ do
        parseJson "{\"a\":2,\"b\":4}" `shouldBe` JObject [("a", JInt 2),("b", JInt 4)]

      it "should return JArray for [[[3]]]" $ do
        parseJson "[[[3]]]" `shouldBe` JArray [JArray [JArray [JInt 3]]]

      it "should return JObject for {a:{b:4},c:-1}" $ do
        parseJson "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` JObject [("a", JObject [("b", JInt 4)]), ("c", JInt (-1))]

      it "should return JObject for Day12Input" $ do
        jsonStr <- getTestInput "Day12Input"
        parseJson jsonStr `shouldSatisfy` (\jval -> case jval of
                                                      (JObject (("e",(JArray _)):_)) -> True
                                                      _ -> False)

      it "should parse test input" $ do
        let jsonStr = "[[{\"a\":1,\"b\":2},3],4,[[5,6],7]]"
        parseJson jsonStr `shouldBe` JArray [
                                       JArray [
                                         JObject [("a", JInt 1),("b", JInt 2)], JInt 3], 
                                         JInt 4, 
                                         JArray [JArray [JInt 5, JInt 6], JInt 7]]

    context "sumJsonInts" $ do
      it "should return 6 for [1,2,3]" $ do
        let jval = parseJson "[1,2,3]"
        sumJsonInts jval `shouldBe` 6

      it "should return 6 for {a:2,b:4}" $ do
        let jval = parseJson "{\"a\":2, \"b\":4}"
        sumJsonInts jval `shouldBe` 6

      it "should return 3 for [[[3]]]" $ do
        let jval = parseJson "[[[3]]]"
        sumJsonInts jval `shouldBe` 3

      it "should return 3 for {a:{b:4},c:-1}" $ do
        let jval = parseJson "{\"a\":{\"b\":4},\"c\":-1}"
        sumJsonInts jval `shouldBe` 3

      it "should return 0 for []" $ do
        let jval = parseJson "[]"
        sumJsonInts jval `shouldBe` 0

      it "should return 0 for {}" $ do
        let jval = parseJson "{}"
        sumJsonInts jval `shouldBe` 0

      it "should return 111754 for Day12Input" $ do
        jsonStr <- getTestInput "Day12Input"
        sumJsonInts (parseJson jsonStr) `shouldBe` 111754 

    context "sumJsonIntsNoRedOs" $ do
      it "should return 0 for {d:red,e:[1,2,3,4],f:5}" $ do
        let jval = parseJson "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"
        sumJsonIntsNoRedOs jval `shouldBe` 0

      it "should return 4 for [1,{c:red,b:2},3]" $ do
        let jval = parseJson "[1,{\"c\":\"red\",\"b\":2},3]"
        sumJsonIntsNoRedOs jval `shouldBe` 4

      it "should return 6 for [1,2,3]" $ do
        let jval = parseJson "[1,2,3]"
        sumJsonIntsNoRedOs jval `shouldBe` 6

      it "should return 6 for [1,red,5]" $ do
        let jval = parseJson "[1,\"red\",5]"
        sumJsonIntsNoRedOs jval `shouldBe` 6

      it "should return 65402 for Day12Input" $ do
        jsonStr <- getTestInput "Day12Input"
        sumJsonIntsNoRedOs (parseJson jsonStr) `shouldBe` 65402


main :: IO ()
main = hspec spec


