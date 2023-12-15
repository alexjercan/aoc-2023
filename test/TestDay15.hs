module TestDay15 (test) where

import Day15
import Test.Hspec

input :: String
input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

test1 :: Expectation
test1 = part1 input `shouldBe` "1320"

test2 :: Expectation
test2 = part2 input `shouldBe` "145"

test :: IO ()
test = hspec $ do
    describe "day15" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
