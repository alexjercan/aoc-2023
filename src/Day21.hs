module Day21 (main, part1, part2) where

import qualified Data.Set as S

type Position = (Int, Int)
type Grid = (S.Set Position, Int)

coords :: String -> Char -> [Position]
coords input char =
    concatMap
        ( \(r, line) ->
            map (\(c, _) -> (r, c)) $
                filter (\(_, c) -> c == char) $
                    zip [0 ..] line
        )
        $ zip [0 ..]
        $ lines input

parse :: String -> (Position, Grid)
parse input = (starting, (S.fromList $ starting : positions, n))
  where
    positions = coords input '.'
    starting = head $ coords input 'S'
    n = length $ lines input

valid :: Grid -> Position -> Bool
valid (ps, n) (r, c) = (r `mod` n, c `mod` n) `S.member` ps

neighbors :: Position -> S.Set Position
neighbors (r, c) = S.fromList [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

visit :: Grid -> S.Set Position -> S.Set Position
visit grid = S.unions . S.map (S.filter (valid grid) . neighbors)

part1 :: String -> String
part1 = show . length . go . parse
  where
    go (starting, grid) = iterate (visit grid) (S.singleton starting) !! 64

part2 :: String -> String
part2 input = show $ case map (xs !!) $ take 3 [n `div` 2, n + n `div` 2 ..] of
    [a, b, c] -> a + n' * (b - a) + n' * (n' - 1) `div` 2 * ((c - b) - (b - a))
    _ -> error "unreachable"
  where
    (starting, grid@(_, n)) = parse input
    xs = map length $ iterate (visit grid) (S.singleton starting)
    n' = 26501365 `div` n

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
