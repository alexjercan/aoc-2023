module TestDay12 (test) where

import Day12
import Test.Hspec

input :: String
input = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"

test1 :: Expectation
test1 = part1 input `shouldBe` "21"

test2 :: Expectation
test2 = part2 input `shouldBe` "525152"

test :: IO ()
test = hspec $ do
    describe "day12" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
