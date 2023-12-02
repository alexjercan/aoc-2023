module Day02 (main) where

import Util.Parser (Parser, parse)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

data Round = Round
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving (Show)

data Game = Game
    { index :: Int
    , rounds :: [Round]
    } deriving (Show)

colorP :: Parser (String, Int)
colorP = do
    n <- read <$> P.many1 P.digit <* P.spaces
    c <- P.string "red" <|> P.string "green" <|> P.string "blue"
    return (c, n)

colorsToRound :: [(String, Int)] -> Round
colorsToRound cs = Round r g b
    where
        r = sum $ map snd $ filter ((== "red") . fst) cs
        g = sum $ map snd $ filter ((== "green") . fst) cs
        b = sum $ map snd $ filter ((== "blue") . fst) cs

roundP :: Parser Round
roundP = do
    cs <- P.sepBy1 colorP (P.char ',' <* P.spaces)
    return $ colorsToRound cs

gameP :: Parser Game
gameP = do
    is <- read <$> (P.string "Game" *> P.spaces *> P.many1 P.digit <* P.string ":" <* P.spaces)
    rs <- P.sepBy1 roundP (P.char ';' <* P.spaces)
    return $ Game is rs

gamesP :: Parser [Game]
gamesP = P.many1 (gameP <* P.spaces) <* P.eof

parseGames :: String -> [Game]
parseGames = parse gamesP

roundValid :: Round -> Bool
roundValid (Round r g b) = r <= 12 && g <= 13 && b <= 14

gameValid :: Game -> Bool
gameValid = all roundValid . rounds

part1 :: String -> String
part1 = show . sum . map index . filter gameValid . parseGames

gameMinColors :: Game -> Round
gameMinColors (Game _ rs) = Round r g b
    where
        r = maximum $ map red rs
        g = maximum $ map green rs
        b = maximum $ map blue rs

power :: Round -> Int
power (Round r g b) = r * g * b

part2 :: String -> String
part2 = show . sum . map (power . gameMinColors) . parseGames

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input

main :: IO ()
main = interact solve
