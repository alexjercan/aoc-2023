module Day17 (main, part1, part2) where

import Data.Char (digitToInt)
import qualified Data.Map as M
import Util.Search (dijkstra)

type Grid = ([[Int]], Int, Int)

data Dir = U | D | L | R deriving (Eq, Ord, Show)
data DState = DState (Int, Int) Dir Int deriving (Eq, Ord, Show)

parse :: String -> Grid
parse input = (m, w, h)
  where
    m = map (map digitToInt) $ lines input
    w = length $ head m
    h = length m

deltas :: DState -> [DState]
deltas (DState (r, c) U n) = [DState (r - 1, c) U (n + 1), DState (r, c - 1) L 1, DState (r, c + 1) R 1]
deltas (DState (r, c) D n) = [DState (r + 1, c) D (n + 1), DState (r, c - 1) L 1, DState (r, c + 1) R 1]
deltas (DState (r, c) L n) = [DState (r, c - 1) L (n + 1), DState (r - 1, c) U 1, DState (r + 1, c) D 1]
deltas (DState (r, c) R n) = [DState (r, c + 1) R (n + 1), DState (r - 1, c) U 1, DState (r + 1, c) D 1]

neighbors :: Grid -> DState -> [(Int, DState)]
neighbors g@(m, _, _) p = map (\p'@(DState (r', c') _ _) -> (m !! r' !! c', p')) $ filter (valid g) $ deltas p
  where
    valid (_, w, h) (DState (r, c) _ n) = r >= 0 && r < h && c >= 0 && c < w && n <= 3

targetP :: Grid -> DState -> Bool
targetP (_, w, h) (DState (r, c) _ _) = r == h - 1 && c == w - 1

answer :: Grid -> M.Map DState Int -> M.Map DState DState -> Int
answer grid dist _ = minimum $ M.filterWithKey (\a _ -> targetP grid a) dist

part1 :: String -> String
part1 input = show $ minimum $ map (\start -> dijkstra start (targetP grid) (neighbors grid) (answer grid)) starts
  where
    grid = parse input
    starts = [DState (0, 0) D 1, DState (0, 0) R 1]

neighbors' :: Grid -> DState -> [(Int, DState)]
neighbors' g@(m, _, _) p@(DState _ d n) = map (\p'@(DState (r', c') _ _) -> (m !! r' !! c', p')) $ filter (valid g) $ deltas p
  where
    valid (_, w, h) (DState (r', c') d' n') = r' >= 0 && r' < h && c' >= 0 && c' < w && (n >= 4 || d == d') && n' <= 10

targetP' :: Grid -> DState -> Bool
targetP' (_, w, h) (DState (r, c) _ n) = r == h - 1 && c == w - 1 && n >= 4

answer' :: Grid -> M.Map DState Int -> M.Map DState DState -> Int
answer' grid dist _ = minimum $ M.filterWithKey (\a _ -> targetP' grid a) dist

part2 :: String -> String
part2 input = show $ minimum $ map (\start -> dijkstra start (targetP' grid) (neighbors' grid) (answer' grid)) starts
  where
    grid = parse input
    starts = [DState (0, 0) D 1, DState (0, 0) R 1]

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
