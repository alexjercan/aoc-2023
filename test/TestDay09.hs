module TestDay09 (test) where

import Day09
import Test.Hspec

input :: String
input = "0 3 6 9 12 15\n 1 3 6 10 15 21\n10 13 16 21 30 45"

test1 :: Expectation
test1 = part1 input `shouldBe` "114"

test2 :: Expectation
test2 = part2 input `shouldBe` "2"

test :: IO ()
test = hspec $ do
    describe "day09" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
