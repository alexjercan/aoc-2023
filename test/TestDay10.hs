module TestDay10 (test) where

import Day10
import Test.Hspec

test1 :: Expectation
test1 = part1 input `shouldBe` "8"
  where
    input = "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."

test2 :: Expectation
test2 = part2 input `shouldBe` "10"
  where
    input = "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"

test :: IO ()
test = hspec $ do
    describe "day10" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
