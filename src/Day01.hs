module Day01 (main) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails, isPrefixOf)
import Data.Maybe

firstAndLastDigitsNum :: String -> Int
firstAndLastDigitsNum input = first * 10 + lst
    where
        digits = filter isDigit input
        first = digitToInt $ head digits
        lst = digitToInt $ last digits

part1 :: String -> String
part1 input = show $ sum $ map firstAndLastDigitsNum $ lines input

tryToDigit :: String -> Maybe Int
tryToDigit [] = Nothing
tryToDigit input@(x:_)
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

firstAndLastDigitsNum2 :: String -> Int
firstAndLastDigitsNum2 input = first * 10 + lst
    where
        first = head $ mapMaybe tryToDigit (tails input)
        lst = head $ mapMaybe tryToDigit (reverse $ tails input)


part2 :: String -> String
part2 input = show $ sum $ map firstAndLastDigitsNum2 $ lines input

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input

main :: IO ()
main = interact solve
