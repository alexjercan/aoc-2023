module Day01 (main, part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe

maybeDigit1 :: String -> Maybe Int
maybeDigit1 [] = Nothing
maybeDigit1 (x : _)
    | isDigit x = Just $ digitToInt x
    | otherwise = Nothing

maybeDigit2 :: String -> Maybe Int
maybeDigit2 [] = Nothing
maybeDigit2 input@(x : _)
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

firstAndLastDigitsNum :: (String -> Maybe Int) -> String -> Int
firstAndLastDigitsNum maybeDigit input = head digits * 10 + last digits
  where
    digits = mapMaybe maybeDigit $ tails input

part1 :: String -> String
part1 input = show $ sum $ map (firstAndLastDigitsNum maybeDigit1) $ lines input

part2 :: String -> String
part2 input = show $ sum $ map (firstAndLastDigitsNum maybeDigit2) $ lines input

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
