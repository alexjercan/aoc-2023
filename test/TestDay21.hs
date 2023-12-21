module TestDay21 (test) where

import Day21
import Test.Hspec

input :: String
input = "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n..........."

test1 :: Expectation
test1 = part1 input `shouldBe` "42"

test2 :: Expectation
test2 = part2 input `shouldBe` "528192899606863"

test :: IO ()
test = hspec $ do
    describe "day21" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
