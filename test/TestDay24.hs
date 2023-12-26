module TestDay24 (test) where

import Day24
import Test.Hspec

input :: String
input = "19, 13, 30 @ -2,  1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @  1, -5, -3"

test1 :: Expectation
test1 = part1 input `shouldBe` "0"

test2 :: Expectation
test2 = (part2 input) >>= shouldBe "47"

test :: IO ()
test = hspec $ do
    describe "day24" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2

