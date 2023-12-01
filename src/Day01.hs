module Day01 (main) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe

tryToDigit1 :: String -> Maybe Int
tryToDigit1 [] = Nothing
tryToDigit1 (x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

firstAndLastDigitsNum :: (String -> Maybe Int) -> String -> Int
firstAndLastDigitsNum tryToDigit input = first * 10 + lst
  where
    first = head $ mapMaybe tryToDigit (tails input)
    lst = head $ mapMaybe tryToDigit (reverse $ tails input)

part1 :: String -> String
part1 input = show $ sum $ map (firstAndLastDigitsNum tryToDigit1) $ lines input

tryToDigit2 :: String -> Maybe Int
tryToDigit2 [] = Nothing
tryToDigit2 input@(x : _)
  | "one" `isPrefixOf` input = Just 1
  | "two" `isPrefixOf` input = Just 2
  | "three" `isPrefixOf` input = Just 3
  | "four" `isPrefixOf` input = Just 4
  | "five" `isPrefixOf` input = Just 5
  | "six" `isPrefixOf` input = Just 6
  | "seven" `isPrefixOf` input = Just 7
  | "eight" `isPrefixOf` input = Just 8
  | "nine" `isPrefixOf` input = Just 9
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

part2 :: String -> String
part2 input = show $ sum $ map (firstAndLastDigitsNum tryToDigit2) $ lines input

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input

main :: IO ()
main = interact solve
