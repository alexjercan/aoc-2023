module TestDay18 (test) where

import Day18
import Test.Hspec

input :: String
input = "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)"

test1 :: Expectation
test1 = part1 input `shouldBe` "62"

test2 :: Expectation
test2 = part2 input `shouldBe` "952408144115"

test :: IO ()
test = hspec $ do
    describe "day18" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
