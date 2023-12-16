module TestDay16 (test) where

import Day16
import Test.Hspec

input :: String
input = ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."

test1 :: Expectation
test1 = part1 input `shouldBe` "46"

test2 :: Expectation
test2 = part2 input `shouldBe` "51"

test :: IO ()
test = hspec $ do
    describe "day16" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
