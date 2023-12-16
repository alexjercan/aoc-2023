module Day16 (main, part1, part2) where

import qualified Data.Map as M
import qualified Data.Set as S

data Cell = Empty | MirrorS | MirrorB | SplitterH | SplitterV deriving (Show, Eq)
data Dir = N | E | S | W deriving (Show, Eq, Ord)

type Grid = (M.Map (Int, Int) Cell, Int, Int)
type Pos = (Int, Int)

parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell '/' = MirrorS
parseCell '\\' = MirrorB
parseCell '-' = SplitterH
parseCell '|' = SplitterV
parseCell _ = error "Invalid cell"

parse :: String -> Grid
parse input = (grid, w, h)
  where
    grid =
        M.fromList $
            concat $
                zipWith (\row cells -> zipWith (\col cell -> ((row, col), parseCell cell)) [0 ..] cells) [0 ..] $
                    lines input
    w = length $ head $ lines input
    h = length $ lines input

move :: Pos -> Dir -> (Pos, Dir)
move (row, col) N = ((row - 1, col), N)
move (row, col) E = ((row, col + 1), E)
move (row, col) S = ((row + 1, col), S)
move (row, col) W = ((row, col - 1), W)

mirrorS :: Pos -> Dir -> (Pos, Dir)
mirrorS (row, col) N = ((row, col + 1), E)
mirrorS (row, col) E = ((row - 1, col), N)
mirrorS (row, col) S = ((row, col - 1), W)
mirrorS (row, col) W = ((row + 1, col), S)

mirrorB :: Pos -> Dir -> (Pos, Dir)
mirrorB (row, col) N = ((row, col - 1), W)
mirrorB (row, col) E = ((row + 1, col), S)
mirrorB (row, col) S = ((row, col + 1), E)
mirrorB (row, col) W = ((row - 1, col), N)

splitterH :: Pos -> Dir -> [(Pos, Dir)]
splitterH (row, col) N = [((row, col - 1), W), ((row, col + 1), E)]
splitterH (row, col) E = [((row, col + 1), E)]
splitterH (row, col) S = [((row, col - 1), W), ((row, col + 1), E)]
splitterH (row, col) W = [((row, col - 1), W)]

splitterV :: Pos -> Dir -> [(Pos, Dir)]
splitterV (row, col) N = [((row - 1, col), N)]
splitterV (row, col) E = [((row - 1, col), N), ((row + 1, col), S)]
splitterV (row, col) S = [((row + 1, col), S)]
splitterV (row, col) W = [((row - 1, col), N), ((row + 1, col), S)]

valid :: Pos -> Grid -> Bool
valid (row, col) (_, w, h) = row >= 0 && row < h && col >= 0 && col < w

step :: Grid -> (Pos, Dir) -> [(Pos, Dir)]
step grid@(m, _, _) (p, dir) = filter ((`valid` grid) . fst) $ case m M.! p of
    Empty -> [move p dir]
    MirrorS -> [mirrorS p dir]
    MirrorB -> [mirrorB p dir]
    SplitterH -> splitterH p dir
    SplitterV -> splitterV p dir

simulate :: (Pos, Dir) -> Grid -> Int
simulate start grid = go (S.singleton start) [start]
  where
    go :: S.Set (Pos, Dir) -> [(Pos, Dir)] -> Int
    go visited [] = length $ S.map fst visited
    go visited lights =
        let
            lights' = concatMap (step grid) lights
            visited' = S.union visited $ S.fromList lights'
            lights'' = filter (`S.notMember` visited) lights'
         in
            go visited' lights''

part1 :: String -> String
part1 = show . simulate ((0, 0), E) . parse

part2 :: String -> String
part2 input = show $ maximum ss
  where
    grid = parse input
    (_, w, h) = grid
    ss =
        [simulate ((row, 0), E) grid | row <- [0 .. h - 1]]
            ++ [simulate ((row, w - 1), W) grid | row <- [0 .. h - 1]]
            ++ [simulate ((0, col), S) grid | col <- [0 .. w - 1]]
            ++ [simulate ((h - 1, col), N) grid | col <- [0 .. w - 1]]

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
