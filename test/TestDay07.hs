module TestDay07 (test) where

import Day07
import Test.Hspec

input :: String
input = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

test1 :: Expectation
test1 = part1 input `shouldBe` "6440"

test2 :: Expectation
test2 = part2 input `shouldBe` "5905"

test :: IO ()
test = hspec $ do
    describe "day07" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
