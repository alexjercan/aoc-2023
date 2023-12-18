module Day18 (main, part1, part2) where

import Control.Arrow ((&&&))
import Numeric (readHex)
import qualified Text.Parsec as P
import Util.Parser (Parser, parse)

data Dir = U | D | L | R deriving (Show, Eq)

data DigPlan = DigPlan Dir Int deriving (Show, Eq)

dirP :: Parser Dir
dirP =
    P.choice
        [ P.char 'U' >> return U
        , P.char 'D' >> return D
        , P.char 'L' >> return L
        , P.char 'R' >> return R
        ]

planP :: Parser DigPlan
planP = do
    d <- dirP <* P.spaces
    l <- read <$> P.many1 P.digit <* P.spaces
    _ <- P.between (P.char '(') (P.char ')') (P.char '#' *> P.many1 P.hexDigit) <* P.spaces
    return $ DigPlan d l

planP' :: Parser DigPlan
planP' = do
    _ <- dirP <* P.spaces
    _ <- P.many1 P.digit <* P.spaces
    c <- P.between (P.char '(') (P.char ')') (P.char '#' *> P.many1 P.hexDigit) <* P.spaces
    let (h, c') = (init &&& last) c
    d <- case c' of
        '0' -> return R
        '1' -> return D
        '2' -> return L
        '3' -> return U
        _ -> P.unexpected "Invalid direction"
    l <- case readHex h of
        [(l, "")] -> return l
        _ -> P.unexpected "Invalid length"
    return $ DigPlan d l

inputP :: Parser [DigPlan]
inputP = P.many1 planP

inputP' :: Parser [DigPlan]
inputP' = P.many1 planP'

step :: (Int, Int) -> DigPlan -> (Int, Int)
step (r, c) (DigPlan U l) = (r - l, c)
step (r, c) (DigPlan D l) = (r + l, c)
step (r, c) (DigPlan L l) = (r, c - l)
step (r, c) (DigPlan R l) = (r, c + l)

area :: [DigPlan] -> Int
area = (1 +) . (`div` 2) . snd . foldl go ((0, 0), 0)
  where
    go ((r, c), a) p =
        let (r', c') = step (r, c) p
         in ((r', c'), a + c * r' - c' * r + abs (r - r') + abs (c - c'))

part1 :: String -> String
part1 = show . area . parse inputP

part2 :: String -> String
part2 = show . area . parse inputP'

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
