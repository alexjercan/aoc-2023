module TestDay17 (test) where

import Day17
import Test.Hspec

input :: String
input = "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"

input' :: String
input' = "111111111111\n999999999991\n999999999991\n999999999991\n999999999991"

test1 :: Expectation
test1 = part1 input `shouldBe` "102"

test2 :: Expectation
test2 = part2 input `shouldBe` "94"

test2' :: Expectation
test2' = part2 input' `shouldBe` "71"

test :: IO ()
test = hspec $ do
    describe "day17" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
            it "should work for the examples 2" test2'
