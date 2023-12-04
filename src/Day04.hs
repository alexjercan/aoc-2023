module Day04 (main, part1, part2) where

import qualified Text.Parsec as P
import Util.Parser (Parser, parse)

data Card = Card [Int] [Int]
    deriving (Show)

cardP :: Parser Card
cardP = do
    _ <- P.string "Card" *> P.spaces *> P.many1 P.digit <* P.char ':' <* P.spaces
    ws <- P.many1 (read <$> P.many1 P.digit <* P.spaces)
    _ <- P.spaces *> P.char '|' <* P.spaces
    ns <- P.many1 (read <$> P.many1 P.digit <* P.spaces)
    pure $ Card ws ns

parseCards :: String -> [Card]
parseCards = parse (P.many1 cardP)

matches :: Card -> Int
matches (Card winning numbers) = length $ filter (`elem` numbers) winning

score :: Int -> Int
score m = if m == 0 then 0 else 2 ^ (m - 1)

part1 :: String -> String
part1 = show . sum . map (score . matches) . parseCards

count :: [Card] -> Int
count cs = sum $ foldl go (replicate (length ms) 1) (zip ms [1 ..])
  where
    ms = map matches cs
    go :: [Int] -> (Int, Int) -> [Int]
    go acc (m, i) = zipWith (\a j -> if j `elem` [i + 1 .. i + m] then a + (acc !! (i - 1)) else a) acc [1 ..]

part2 :: String -> String
part2 = show . count . parseCards

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
