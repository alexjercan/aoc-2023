module Day03 (main, part1, part2) where

import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as M

type Map = [[Char]]

parse :: String -> Map
parse = lines

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

at :: Map -> (Int, Int) -> Char
at m (x, y) = m !! y !! x

blanket :: Map -> (Int, Int) -> [(Int, Int)]
blanket m (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0, x + dx >= 0, x + dx < width m, y + dy >= 0, y + dy < height m]

around :: Map -> (Int, Int) -> [Char]
around m (x, y) = map (at m) $ blanket m (x, y)

isSymbol :: Char -> Bool
isSymbol = (&&) <$> (/= '.') <*> (not . isDigit)

solveLine :: Map -> Int -> [(Int, Char, (Int, Int))]
solveLine m y =
    map generate $
        filter hasSymbol $
            uncurry zip $
                result $
                    foldl readNumber (0, [], [], []) indices
  where
    w = width m
    indices = [0 .. w - 1]
    readNumber :: (Int, [Int], [Int], [[Int]]) -> Int -> (Int, [Int], [Int], [[Int]])
    readNumber (c, acc, cs, xs) x
        | null cs && not (isDigit (at m (x, y))) = (c, acc, cs, xs)
        | not (isDigit (at m (x, y))) = (0, c : acc, [], cs : xs)
        | otherwise = (c * 10 + read [at m (x, y)], acc, x : cs, xs)
    result :: (Int, [Int], [Int], [[Int]]) -> ([Int], [[Int]])
    result (c, acc, cs, xs)
        | null cs = (acc, xs)
        | otherwise = (c : acc, cs : xs)
    hasSymbol :: (Int, [Int]) -> Bool
    hasSymbol (_, cs) = any (\x -> any isSymbol (around m (x, y))) cs
    generate :: (Int, [Int]) -> (Int, Char, (Int, Int))
    generate (c, cs) = (c, at m pos, pos)
      where
        bs = nub $ concatMap (\x -> blanket m (x, y)) cs
        pos = head $ filter (isSymbol . at m) bs

part1 :: String -> String
part1 = show . sum . solveMap . parse
  where
    solveMap :: Map -> [Int]
    solveMap m = map (\(a, _, _) -> a) $ concatMap (solveLine m) [0 .. height m - 1]

part2 :: String -> String
part2 = show . sum . solveMap . parse
  where
    solveMap :: Map -> [Int]
    solveMap m =
        map product $
            filter validGear $
                M.elems $
                    foldl buildGears M.empty $
                        concatMap (filter checkGear . solveLine m) [0 .. height m - 1]
    checkGear :: (Int, Char, (Int, Int)) -> Bool
    checkGear (_, g, _) = g == '*'
    buildGears :: M.Map (Int, Int) [Int] -> (Int, Char, (Int, Int)) -> M.Map (Int, Int) [Int]
    buildGears m (c, _, pos) = M.insertWith (++) pos [c] m
    validGear :: [Int] -> Bool
    validGear gs = length gs == 2

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input

main :: IO ()
main = interact solve
