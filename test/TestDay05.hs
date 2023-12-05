module TestDay05 where

import Day05
import Test.Hspec

input =
    unlines
        [ "seeds: 79 14 55 13"
        , ""
        , "seed-to-soil map:"
        , "50 98 2"
        , "52 50 48"
        , ""
        , "soil-to-fertilizer map:"
        , "0 15 37"
        , "37 52 2"
        , "39 0 15"
        , ""
        , "fertilizer-to-water map:"
        , "49 53 8"
        , "0 11 42"
        , "42 0 7"
        , "57 7 4"
        , ""
        , "water-to-light map:"
        , "88 18 7"
        , "18 25 70"
        , ""
        , "light-to-temperature map:"
        , "45 77 23"
        , "81 45 19"
        , "68 64 13"
        , ""
        , "temperature-to-humidity map:"
        , "0 69 1"
        , "1 0 69"
        , ""
        , "humidity-to-location map:"
        , "60 56 37"
        , "56 93 4"
        ]

test1 :: Expectation
test1 = part1 input `shouldBe` "35"

test2 :: Expectation
test2 = part2 input `shouldBe` "46"

test :: IO ()
test = hspec $ do
    describe "day05" $ do
        describe "part1" $ do
            it "should work for the examples" test1

        describe "part2" $ do
            it "should work for the examples" test2
