module TestDay22 (test) where

import Day22
import Test.Hspec

input :: String
input = "1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9"

test1 :: Expectation
test1 = part1 input `shouldBe` "5"

test2 :: Expectation
test2 = part2 input `shouldBe` "7"

test :: IO ()
test = hspec $ do
    describe "day22" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
