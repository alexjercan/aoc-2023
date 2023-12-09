module Day09 (main, part1, part2) where

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diff :: [Int] -> [Int]
diff = zipWith (-) <$> tail <*> id

prediction :: (Int -> [Int] -> Int) -> [Int] -> Int
prediction f xs = foldl f 0 $ fst $ until stop step ([], xs)
  where
    step (ps, ys) = (ys : ps, diff ys)
    stop (_, ys) = all (== 0) ys

part1 :: String -> String
part1 = show . sum . map (prediction merge) . parse
    where merge acc xs = acc + last xs

part2 :: String -> String
part2 = show . sum . map (prediction merge) . parse
    where merge acc xs = head xs - acc

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
