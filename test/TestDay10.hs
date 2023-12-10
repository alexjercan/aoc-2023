module TestDay10 (test) where

import Day10
import Test.Hspec

input :: String
input = "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."

test1 :: Expectation
test1 = part1 input `shouldBe` "8"

test2 :: Expectation
test2 = part2 input `shouldBe` "10"

test :: IO ()
test = hspec $ do
    describe "day10" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
