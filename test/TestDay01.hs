module TestDay01 where

import Test.Hspec
import Day01

test1 :: Expectation
test1 = part1 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" `shouldBe` "142"

test2 :: Expectation
test2 = part2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" `shouldBe` "281"

test :: IO ()
test = hspec $ do
  describe "part1" $ do
    it "should work for the examples" test1

  describe "part2" $ do
    it "should work for the examples" test2
