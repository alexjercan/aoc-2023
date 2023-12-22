module Day22 (main, part1, part2) where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad.State (State, evalState, get, put)
import Data.List (intersect, sortBy)
import qualified Data.Map as M
import qualified Text.Parsec as P
import Util.Parser (Parser, parse)

type Block = (Int, Int, Int)

data Brick = Brick Block Block deriving (Show, Eq, Ord)

blockP :: Parser Block
blockP = do
    x <- read <$> P.many1 P.digit <* P.char ','
    y <- read <$> P.many1 P.digit <* P.char ','
    z <- read <$> P.many1 P.digit
    return (x, y, z)

brickP :: Parser Brick
brickP = do
    a <- blockP <* P.string "~"
    b <- blockP <* P.spaces
    return $ Brick a b

inputP :: Parser [Brick]
inputP = P.many1 brickP <* P.eof

below :: Brick -> Brick -> Bool
below b@(Brick (_, _, z1) (_, _, z2)) b'@(Brick (_, _, z1') (_, _, z2')) = max z1 z2 == min z1' z2' - 1 && b /= b'

hold :: Brick -> Brick -> Bool
hold b b' = below b b' && not (null $ intersect (squares b) (squares b'))
  where
    squares (Brick (x1, y1, _) (x2, y2, _)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

blocked :: Brick -> [Brick] -> Bool
blocked n m = bottom n || any (`hold` n) m
  where
    bottom (Brick (_, _, z1) (_, _, z2)) = min z1 z2 == 1

move :: Brick -> Brick
move (Brick (x1, y1, z1) (x2, y2, z2)) = Brick (x1, y1, z1 - 1) (x2, y2, z2 - 1)

updatePlane :: [Brick] -> Brick -> [Brick]
updatePlane ms m = (if blocked m ms then m else move m) : ms

step :: [Brick] -> [Brick]
step = reverse . foldl updatePlane [] . sortBricks
  where
    sortBricks :: [Brick] -> [Brick]
    sortBricks = sortBy (\(Brick (_, _, z1) (_, _, z2)) (Brick (_, _, z3) (_, _, z4)) -> compare (min z1 z2) (min z3 z4))

check :: [Brick] -> Bool
check m = m == step m

simulate :: [Brick] -> [Brick]
simulate = until check step

support :: [Brick] -> (M.Map Brick [Brick], M.Map Brick [Brick])
support bs = (go &&& go') bs
  where
    go :: [Brick] -> M.Map Brick [Brick]
    go [] = M.empty
    go (b : bs') = M.insert b (filter (`hold` b) bs) $ go bs'
    go' :: [Brick] -> M.Map Brick [Brick]
    go' [] = M.empty
    go' (b : bs') = M.insert b (filter (b `hold`) bs) $ go' bs'

redundant :: [Brick] -> Int
redundant bs = length $ filter (\(_, ss) -> all (\s -> length (supported M.! s) > 1) ss) $ M.toList supports
  where
    (supported, supports) = support bs

type ChainState = State (M.Map Brick [Brick], M.Map Brick [Brick])

chain :: Brick -> ChainState Int
chain b = do
    (supported, supports) <- get
    if null (supported M.! b)
        then do
            put (M.map (filter (/= b)) supported, supports)
            (+ 1) . sum <$> mapM chain (supports M.! b)
        else return 0

clear :: Brick -> ChainState ()
clear b = do
    (supported, supports) <- get
    put (M.insert b [] supported, supports)
    return ()

reaction :: [Brick] -> Int
reaction bs = sum $ map (\b -> (-1) + evalState (clear b >> chain b) (support bs)) bs

part1 :: String -> String
part1 = show . redundant . simulate . parse inputP

part2 :: String -> String
part2 = show . reaction . simulate . parse inputP

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
