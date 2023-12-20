module TestDay20 (test) where

import Day20
import Test.Hspec

input :: String
input = "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a"

test1 :: Expectation
test1 = part1 input `shouldBe` "32000000"

test :: IO ()
test = hspec $ do
    describe "day20" $ do
        describe "part1" $ do
            it "should work for the examples" test1
