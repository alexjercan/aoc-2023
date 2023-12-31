module Day23 (main, part1, part2) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

data Dir = C | U | D | L | R deriving (Show, Eq, Ord)

charToDir :: Char -> Dir
charToDir '^' = U
charToDir 'v' = D
charToDir '<' = L
charToDir '>' = R
charToDir '.' = C
charToDir _ = error "Invalid direction"

type Position = (Int, Int)
type Grid = (M.Map Position Dir, Int)

coords :: String -> (Char -> Bool) -> [(Position, Char)]
coords input p =
    concatMap
        ( \(r, line) ->
            map (\(c, c') -> ((r, c), c')) $
                filter (\(_, c') -> p c') $
                    zip [0 ..] line
        )
        $ zip [0 ..]
        $ lines input

parse :: String -> ((Position, Dir), (Position, Dir), Grid)
parse input = (starting, target, (M.fromList positions, n))
  where
    positions = map (fmap charToDir) $ coords input (/= '#')
    starting = head $ filter ((== 0) . fst . fst) positions
    target = head $ filter ((== n - 1) . fst . fst) positions
    n = length $ lines input

parse' :: String -> ((Position, Dir), (Position, Dir), Grid)
parse' input = (starting, target, (M.fromList positions, n))
  where
    positions = map (fmap (const C)) $ coords input (/= '#')
    starting = head $ filter ((== 0) . fst . fst) positions
    target = head $ filter ((== n - 1) . fst . fst) positions
    n = length $ lines input

neighbors :: Grid -> (Position, Dir) -> [(Position, Dir)]
neighbors (grid, _) ((r, c), U) = [(p, d) | p <- [(r - 1, c)], Just d <- [M.lookup p grid]]
neighbors (grid, _) ((r, c), D) = [(p, d) | p <- [(r + 1, c)], Just d <- [M.lookup p grid]]
neighbors (grid, _) ((r, c), L) = [(p, d) | p <- [(r, c - 1)], Just d <- [M.lookup p grid]]
neighbors (grid, _) ((r, c), R) = [(p, d) | p <- [(r, c + 1)], Just d <- [M.lookup p grid]]
neighbors (grid, _) ((r, c), C) = [(p, d) | p <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)], Just d <- [M.lookup p grid]]

edge :: Grid -> (Position, Dir) -> (Position, Dir) -> Int -> ((Position, Dir) -> [(Position, Dir)]) -> S.Set (Position, Dir) -> Maybe ((Position, Dir), Int)
edge grid t p l neighborsF visited
    | p == t = Just (p, l)
    | p `S.member` visited = Nothing
    | otherwise = case filter (`S.notMember` visited) $ neighborsF p of
        [] -> Nothing
        [n] -> edge grid t n (l + 1) neighborsF (S.insert p visited)
        _ -> Just (p, l)

type GState = State (S.Set (Position, Dir))

graphM :: Grid -> (Position, Dir) -> (Position, Dir) -> ((Position, Dir) -> [(Position, Dir)]) -> GState (M.Map (Position, Dir) [((Position, Dir), Int)])
graphM grid t p neighborsF = do
    visited <- get
    if p `S.member` visited
        then return M.empty
        else do
            put $ S.insert p visited
            case mapMaybe (\n -> edge grid t n 1 neighborsF (S.singleton p)) (neighborsF p) of
                [] -> return M.empty
                ns -> do
                    m <- M.unions <$> mapM (\(n, _) -> graphM grid t n neighborsF) ns
                    return $ M.insert p ns m

dfs :: (Position, Dir) -> M.Map (Position, Dir) [((Position, Dir), Int)] -> (Position, Dir) -> [(Position, Dir)] -> Int -> Int
dfs current graph target visited maxLength =
    let ns = filter (\(neighbor, _) -> neighbor `notElem` visited) $ M.findWithDefault [] current graph
     in if null ns || current == target
            then maxLength
            else
                maximum $
                    [dfs neighbor graph target (neighbor : visited) (maxLength + weight) | (neighbor, weight) <- ns]

longestPath :: M.Map (Position, Dir) [((Position, Dir), Int)] -> (Position, Dir) -> (Position, Dir) -> Int
longestPath graph start end = dfs start graph end [start] 0

part1 :: String -> String
part1 input = show $ longestPath gs starting target
  where
    (starting, target, grid) = parse input
    gs = evalState (graphM grid target starting (neighbors grid)) S.empty

part2 :: String -> String
part2 input = show $ longestPath gs starting target
  where
    (starting, target, grid) = parse' input
    gs = evalState (graphM grid target starting (neighbors grid)) S.empty

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
