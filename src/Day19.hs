module Day19 (main, part1, part2) where

part1 :: String -> String
part1 = id

part2 :: String -> String
part2 = const ""

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
