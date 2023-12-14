module Day14 (main, part1, part2) where

import Data.List (transpose)

foldlUntil :: (a -> b -> Bool) -> (a -> b -> a) -> a -> [b] -> a
foldlUntil _ _ z [] = z
foldlUntil p f z (x : xs) = if p z x then z else foldlUntil p f (f z x) xs

updateCellUp :: Char -> Char -> Char
updateCellUp '.' '.' = '.'
updateCellUp '#' x = x
updateCellUp _ '#' = '#'
updateCellUp 'O' '.' = 'O'
updateCellUp _ y = y

updateCellDown :: Char -> Char -> Char
updateCellDown '.' '.' = '.'
updateCellDown '#' _ = '#'
updateCellDown x '#' = x
updateCellDown 'O' '.' = '.'
updateCellDown x _ = x

updateRow :: [String] -> String -> [String]
updateRow [] _ = []
updateRow (m : ms) n = zipWith updateCellDown n m : zipWith updateCellUp n m : ms

step :: [String] -> [String]
step [] = []
step (m : ms) = reverse $ foldl updateRow [m] ms

check :: [String] -> Bool
check m = m == step m

simulate :: [String] -> [String]
simulate = until check step

load :: [String] -> Int
load = sum . zipWith (*) [1 ..] . map (length . filter (== 'O')) . reverse

part1 :: String -> String
part1 = show . load . simulate . lines

spinCycle :: [String] -> [String]
spinCycle =
    (transpose . reverse)
        . (simulate . transpose . reverse)
        . (simulate . transpose . reverse)
        . (simulate . transpose . reverse)
        . simulate

findCycle :: (Eq a) => [a] -> ([a], [a])
findCycle xs = break (== y) ys
  where
    ys = reverse $ foldlUntil (flip elem) (flip (:)) [] xs
    y = xs !! length ys

part2 :: String -> String
part2 input = show $ load $ cs !! ((1000000000 - length pre) `mod` length cs)
  where
    (pre, cs) = findCycle $ iterate spinCycle $ lines input

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
