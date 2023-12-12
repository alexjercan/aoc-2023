{-# LANGUAGE LambdaCase #-}

module Day12 (main, part1, part2) where

import Data.List (intercalate, tails)
import Data.List.Split (splitOn)
import qualified Data.Map as M

data Symbol = Any | Hash | Dot deriving (Eq, Show, Ord)

charToSymbol :: Char -> Symbol
charToSymbol '?' = Any
charToSymbol '#' = Hash
charToSymbol '.' = Dot
charToSymbol _ = error "Invalid input"

parse :: String -> [([Symbol], [Int])]
parse =
    map
        ( ( \case
                [config, counts] -> (map charToSymbol config, map read $ splitOn "," counts)
                _ -> error "Invalid input"
          )
            . words
        )
        . lines

configure :: ([Symbol], [Int]) -> Int
configure (symbols, counts) = dp M.! (0, symbols, counts)
  where
    dp = M.fromList [((g, s, c), consume g s c) | g <- [0 .. maximum counts], s <- tails symbols, c <- tails counts]
    consume :: Int -> [Symbol] -> [Int] -> Int
    consume 0 s [] = if all ((||) <$> (== Dot) <*> (== Any)) s then 1 else 0
    consume _ _ [] = 0
    consume g [] [c] = if g == c then 1 else 0
    consume _ [] _ = 0
    consume g (Hash : xs) cs'@(c : _) = if g < c then dp M.! (g + 1, xs, cs') else 0
    consume 0 (Dot : xs) cs = dp M.! (0, xs, cs)
    consume g (Dot : xs) (c : cs) = if g == c then dp M.! (0, xs, cs) else 0
    consume 0 (Any : xs) cs = dp M.! (1, xs, cs) + dp M.! (0, xs, cs)
    consume g (Any : xs) cs'@(c : cs)
        | g == c = dp M.! (0, xs, cs)
        | g < c = dp M.! (g + 1, xs, cs')
        | otherwise = 0

part1 :: String -> String
part1 = show . sum . map configure . parse

join :: Int -> a -> ([a], [Int]) -> ([a], [Int])
join times a (symbols, counts) = (intercalate [a] $ replicate times symbols, concat $ replicate times counts)

part2 :: String -> String
part2 = show . sum . map (configure . join 5 Any) . parse

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
