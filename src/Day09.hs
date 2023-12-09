module Day09 (main, part1, part2) where

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diff :: [Int] -> [Int]
diff = zipWith (-) <$> tail <*> id

prediction :: [Int] -> Int
prediction xs = fst $ until stop step (0, xs)
  where
    step :: (Int, [Int]) -> (Int, [Int])
    step (p, ys) = (p + last ys, diff ys)
    stop :: (Int, [Int]) -> Bool
    stop (_, ys) = all (== 0) ys

part1 :: String -> String
part1 = show . sum . map prediction . parse

prediction' :: [Int] -> Int
prediction' xs = foldl merge 0 $ fst $ until stop step ([], xs)
  where
    step :: ([Int], [Int]) -> ([Int], [Int])
    step (ps, ys) = (head ys : ps, diff ys)
    stop :: ([Int], [Int]) -> Bool
    stop (_, ys) = all (== 0) ys
    merge :: Int -> Int -> Int
    merge acc x = x - acc

part2 :: String -> String
part2 = show . sum . map prediction' . parse

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
