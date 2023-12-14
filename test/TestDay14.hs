module TestDay14 (test) where

import Day14
import Test.Hspec

input :: String
input = "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."

test1 :: Expectation
test1 = part1 input `shouldBe` "136"

test2 :: Expectation
test2 = part2 input `shouldBe` "64"

test :: IO ()
test = hspec $ do
    describe "day14" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
