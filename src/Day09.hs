module Day09 (main, part1, part2) where

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diff :: [Int] -> [Int]
diff = zipWith (-) <$> tail <*> id

prediction :: [Int] -> Int
prediction xs = fst $ until stop step (0, xs)
  where
    step (p, ys) = (p + last ys, diff ys)
    stop (_, ys) = all (== 0) ys

part1 :: String -> String
part1 = show . sum . map prediction . parse

part2 :: String -> String
part2 = show . sum . map (prediction . reverse) . parse

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
