{-# LANGUAGE TupleSections #-}

module Day11 (main, part1, part2) where

import Control.Arrow ((&&&))
import Data.List (transpose)

empty :: String -> Bool
empty = all (== '.')

emptyRows :: [String] -> [Int]
emptyRows = map fst . filter (empty . snd) . zip [0 ..]

emptyCols :: [String] -> [Int]
emptyCols = emptyRows . transpose

expanded :: [String] -> ([Int], [Int])
expanded m = (emptyRows m, emptyCols m)

galaxies :: [String] -> [(Int, Int)]
galaxies = concat . zipWith go [0 ..]
  where
    go r = map ((r,) . fst) . filter (\(_, x) -> x == '#') . zip [0 ..]

parse :: String -> (([Int], [Int]), [(Int, Int)])
parse = (expanded &&& galaxies) . lines

pairs :: (Eq a) => [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

manhattan :: [Int] -> [Int] -> Int -> (Int, Int) -> (Int, Int) -> Int
manhattan rows cols f (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) + (rowInts + colInts) * (f - 1)
  where
    rowInts = length $ filter (\r -> min r1 r2 <= r && r <= max r1 r2) rows
    colInts = length $ filter (\c -> min c1 c2 <= c && c <= max c1 c2) cols

solution :: Int -> String -> Int
solution f input = sum $ map (uncurry path) ps
  where
    ((rs, cs), gs) = parse input
    ps = pairs gs
    path = manhattan rs cs f

part1 :: String -> String
part1 = show . solution 2

part2 :: String -> String
part2 = show . solution 1000000

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
