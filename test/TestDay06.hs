module TestDay06 (test) where

import Day06
import Test.Hspec

input :: String
input = "Time:      7  15   30\nDistance:  9  40  200"

test1 :: Expectation
test1 = part1 input `shouldBe` "288"

test2 :: Expectation
test2 = part2 input `shouldBe` "71503"

test :: IO ()
test = hspec $ do
    describe "day06" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
