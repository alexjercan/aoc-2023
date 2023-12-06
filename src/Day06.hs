module Day06 (main, part1, part2) where

data Race = Race Int Int deriving (Show, Eq)

input = "Time:      7  15   30\nDistance:  9  40  200"

parse :: String -> [Race]
parse input = case lines input of
    [times, distances] -> zipWith Race (map read $ tail $ words times) (map read $ tail $ words distances)
    _ -> error "Wrong input"

parse' :: String -> Race
parse' input = case lines input of
    [time, distance] -> Race (read $ concat $ tail $ words time) (read $ concat $ tail $ words distance)
    _ -> error "Wrong input"

nextInt :: Double -> Int
nextInt x = ceiling x + if x == fromIntegral (ceiling x) then 1 else 0

prevInt :: Double -> Int
prevInt x = floor x - if x == fromIntegral (floor x) then 1 else 0

limits :: Race -> Int
limits (Race t d) = prevInt t2 - nextInt t1 + 1
    where
        delta = sqrt $ fromIntegral $ t * t - 4 * d
        t1 = (fromIntegral t - delta ) / 2
        t2 = (fromIntegral t + delta ) / 2

part1 :: String -> String
part1 = show . product . map limits . parse

part2 :: String -> String
part2 = show . limits . parse'

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
