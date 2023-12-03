module TestDay02 where

import Day02
import Test.Hspec

input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

test1 :: Expectation
test1 = part1 input `shouldBe` "8"

test2 :: Expectation
test2 = part2 input `shouldBe` "2286"

test :: IO ()
test = hspec $ do
    describe "part1" $ do
        it "should work for the examples" test1

    describe "part2" $ do
        it "should work for the examples" test2
