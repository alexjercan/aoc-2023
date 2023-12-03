module TestDay03 where

import Day03
import Test.Hspec

input = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

test1 :: Expectation
test1 = part1 input `shouldBe` "4361"

test2 :: Expectation
test2 = part2 input `shouldBe` "467835"

test :: IO ()
test = hspec $ do
    describe "part1" $ do
        it "should work for the examples" test1

    describe "part2" $ do
        it "should work for the examples" test2
