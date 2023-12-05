module Day05 (main, part1, part2) where

import Data.List.Split (splitOn)

data MapItem = MapItem { dest :: Int, src :: Int, len :: Int } deriving (Show)

parseMapItem :: String -> MapItem
parseMapItem line = case map read $ words line of
    [d, s, l] -> MapItem d s l
    _ -> error "Invalid input"

parseMap :: String -> [MapItem]
parseMap = map parseMapItem . tail . lines

parseMaps :: String -> ([Int], [[MapItem]])
parseMaps input = case splitOn "\n\n" input of
    (x:ls) -> (map read $ tail $ words  x, map parseMap ls)
    _ -> error "Invalid input"

mapRange :: Int -> [MapItem] -> Int
mapRange x [] = x
mapRange x (MapItem d s l:xs)
    | s <= x && x < s + l = d + x - s
    | otherwise = mapRange x xs

part1 :: String -> String
part1 input = show $ minimum $ map (\x -> foldl mapRange x maps) seeds
    where (seeds, maps) = parseMaps input

mapRange' :: (Int, Int) -> [MapItem] -> [(Int, Int)]
mapRange' x [] = [x]
mapRange' (rs, rl) (MapItem d s l:ms)
    | rs <= s + l && s < rs + rl = pre ++ curr ++ post
    | otherwise = mapRange' (rs, rl) ms
        where
            pre = if rs < s then mapRange' (rs, s - rs) ms else []
            curr = [(d + max 0 (rs - s), min rl (l - max 0 (rs - s)))]
            post = if s + l < rs + rl then mapRange' (s + l, rs + rl - s - l) ms else []

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x:y:rest) = (x, y) : pairUp rest
pairUp _ = error "Input list should have an even number of elements."

part2 :: String -> String
part2 input = show $ fst $ minimum $ foldl mapRange'' (pairUp seeds) maps
    where
        (seeds, maps) = parseMaps input
        mapRange'' :: [(Int, Int)] -> [MapItem] -> [(Int, Int)]
        mapRange'' xs ms = concatMap (`mapRange'` ms) xs


solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
