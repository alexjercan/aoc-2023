module Day13 (main, part1, part2) where

import Data.Bifunctor (first)
import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

reflection :: (Eq a) => (Int -> Bool) -> [[a]] -> Maybe Int
reflection f xs = find mirror [1 .. length xs - 1]
  where
    mirror i =
        f $
            sum $
                uncurry (zipWith (curry (sum . uncurry (zipWith check)))) $
                    first reverse $
                        splitAt i xs
    check a b = if a == b then 0 else 1

reflexiveV :: (Eq a) => (Int -> Bool) -> [[a]] -> Maybe Int
reflexiveV = reflection

reflexiveVr :: (Eq a) => (Int -> Bool) -> [[a]] -> Maybe Int
reflexiveVr f = reflexiveV f . map reverse

reflexiveH :: (Eq a) => (Int -> Bool) -> [[a]] -> Maybe Int
reflexiveH f = reflexiveV f . transpose

reflexiveHr :: (Eq a) => (Int -> Bool) -> [[a]] -> Maybe Int
reflexiveHr f = reflexiveV f . map reverse . transpose

reflexive :: (Eq a) => (Int -> Bool) -> [[a]] -> Int
reflexive f xs =
    fromJust $
        head $
            filter
                (/= Nothing)
                [ (*) 100 <$> reflexiveV f xs
                , (*) 100 <$> reflexiveVr f xs
                , reflexiveH f xs
                , reflexiveHr f xs
                ]

part1 :: String -> String
part1 = show . sum . map (reflexive (0 ==)) . parse

part2 :: String -> String
part2 = show . sum . map (reflexive (1 ==)) . parse

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
