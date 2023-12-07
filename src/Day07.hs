module Day07 (main, part1, part2) where

import Data.List (group, sort, sortBy)

data Hand = Hand String Int deriving (Show, Eq)

rank :: String -> Int
rank hand = case sortBy (flip compare) $ map length $ group $ sort hand of
    [5] -> 7
    [4, _] -> 6
    [3, 2] -> 5
    [3, _, _] -> 4
    [2, 2, _] -> 3
    [2, _, _, _] -> 2
    _ -> 1

jokers :: String -> Int
jokers = length . filter (== '0')

rank' :: String -> Int
rank' hand = case (jokers hand, sortBy (flip compare) $ map length $ group $ sort hand) of
    (_, [5]) -> 7
    (4, [4, _]) -> 7
    (1, [4, _]) -> 7
    (_, [4, _]) -> 6
    (3, [3, 2]) -> 7
    (2, [3, 2]) -> 7
    (_, [3, 2]) -> 5
    (3, [3, _, _]) -> 6
    (1, [3, _, _]) -> 6
    (_, [3, _, _]) -> 4
    (2, [2, 2, _]) -> 6
    (1, [2, 2, _]) -> 5
    (_, [2, 2, _]) -> 3
    (2, [2, _, _, _]) -> 4
    (1, [2, _, _, _]) -> 4
    (_, [2, _, _, _]) -> 2
    (1, _) -> 2
    (_, _) -> 1

compareHand :: Hand -> Hand -> Ordering
compareHand (Hand h1 _) (Hand h2 _)
    | rank h1 > rank h2 = GT
    | rank h1 < rank h2 = LT
    | otherwise = compare h1 h2

compareHand' :: Hand -> Hand -> Ordering
compareHand' (Hand h1 _) (Hand h2 _)
    | rank' h1 > rank' h2 = GT
    | rank' h1 < rank' h2 = LT
    | otherwise = compare h1 h2

formatCards :: String -> String
formatCards = map charToCard
  where
    charToCard 'T' = 'A'
    charToCard 'J' = 'B'
    charToCard 'Q' = 'C'
    charToCard 'K' = 'D'
    charToCard 'A' = 'E'
    charToCard c = c

formatCards' :: String -> String
formatCards' = map charToCard
  where
    charToCard 'T' = 'A'
    charToCard 'J' = '0'
    charToCard 'Q' = 'C'
    charToCard 'K' = 'D'
    charToCard 'A' = 'E'
    charToCard c = c

parseHand :: (String -> String) -> String -> Hand
parseHand format input = case words input of
    [hand, bid] -> Hand (format hand) (read bid)
    _ -> error "Invalid input"

parse :: (String -> String) -> String -> [Hand]
parse format = map (parseHand format) . lines

totalWin :: [Hand] -> Int
totalWin = snd . foldl (\(i, total) (Hand _ bid) -> (i + 1, total + i * bid)) (1, 0)

part1 :: String -> String
part1 = show . totalWin . sortBy compareHand . parse formatCards

part2 :: String -> String
part2 = show . totalWin . sortBy compareHand' . parse formatCards'

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
