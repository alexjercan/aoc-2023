module Day08 (main, part1, part2) where

import qualified Data.Map as M
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Util.Parser (Parser, parse)

lineP :: Parser (String, (String, String))
lineP = do
    start <- P.many1 (P.letter <|> P.digit) <* P.spaces
    _ <- P.char '=' <* P.spaces
    [left, right] <-
        P.between
            (P.char '(')
            (P.char ')')
            (P.sepBy (P.spaces *> P.many1 (P.letter <|> P.digit) <* P.spaces) (P.char ','))
    return (start, (left, right))

inputP :: Parser (String, M.Map String (String, String))
inputP = do
    directions <- P.many1 P.letter <* P.spaces
    m <- M.fromList <$> P.many1 (lineP <* P.spaces)
    return (directions, m)

parseInput :: String -> (String, M.Map String (String, String))
parseInput = parse inputP

step :: M.Map String (String, String) -> String -> Char -> String
step m state c = case M.lookup state m of
    Just (left, right) -> if c == 'L' then left else right
    Nothing -> error "Invalid state"

foldlUntil :: (a -> Bool) -> (a -> b -> a) -> a -> [b] -> a
foldlUntil _ _ z [] = z
foldlUntil p f z (x : xs) = if p z then z else foldlUntil p f (f z x) xs

simulate :: String -> (String -> Bool) -> String -> M.Map String (String, String) -> Int
simulate start endP directions m = fst $ foldlUntil (endP . snd) (step' m) (0, start) $ cycle directions
  where
    step' :: M.Map String (String, String) -> (Int, String) -> Char -> (Int, String)
    step' m' (n, s) c = (n + 1, step m' s c)

startPoints :: M.Map String (String, String) -> [String]
startPoints m = filter ((== 'A') . last) $ M.keys m

part1 :: String -> String
part1 = show . uncurry (simulate "AAA" (== "ZZZ")) . parseInput

part2 :: String -> String
part2 input = show $ foldl lcm 1 $ map simulate' starts
  where
    (directions, m) = parseInput input
    starts = startPoints m
    simulate' s = simulate s ((== 'Z') . last) directions m

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
