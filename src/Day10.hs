module Day10 (main, part1, part2) where

import qualified Data.Map as M

findS :: [String] -> (Int, Int)
findS input = head $ filter (\(x, y) -> (input !! y) !! x == 'S') [(x, y) | x <- [0 .. length (head input) - 1], y <- [0 .. length input - 1]]

checkPipe :: Char -> Char -> Char -> Bool
checkPipe '|' '|' 'N' = True
checkPipe '|' '7' 'N' = True
checkPipe '|' 'F' 'N' = True
checkPipe '|' 'S' 'N' = True
checkPipe '|' '|' 'S' = True
checkPipe '|' 'L' 'S' = True
checkPipe '|' 'J' 'S' = True
checkPipe '|' 'S' 'S' = True
checkPipe '-' '-' 'E' = True
checkPipe '-' 'J' 'E' = True
checkPipe '-' '7' 'E' = True
checkPipe '-' 'S' 'E' = True
checkPipe '-' '-' 'W' = True
checkPipe '-' 'L' 'W' = True
checkPipe '-' 'F' 'W' = True
checkPipe '-' 'S' 'W' = True
checkPipe 'L' '|' 'N' = True
checkPipe 'L' '7' 'N' = True
checkPipe 'L' 'F' 'N' = True
checkPipe 'L' 'S' 'N' = True
checkPipe 'L' '-' 'E' = True
checkPipe 'L' 'J' 'E' = True
checkPipe 'L' '7' 'E' = True
checkPipe 'L' 'S' 'E' = True
checkPipe 'J' '|' 'N' = True
checkPipe 'J' '7' 'N' = True
checkPipe 'J' 'F' 'N' = True
checkPipe 'J' 'S' 'N' = True
checkPipe 'J' '-' 'W' = True
checkPipe 'J' 'L' 'W' = True
checkPipe 'J' 'F' 'W' = True
checkPipe 'J' 'S' 'W' = True
checkPipe '7' '|' 'S' = True
checkPipe '7' 'L' 'S' = True
checkPipe '7' 'J' 'S' = True
checkPipe '7' 'S' 'S' = True
checkPipe '7' '-' 'W' = True
checkPipe '7' 'L' 'W' = True
checkPipe '7' 'F' 'W' = True
checkPipe '7' 'S' 'W' = True
checkPipe 'F' '|' 'S' = True
checkPipe 'F' 'L' 'S' = True
checkPipe 'F' 'J' 'S' = True
checkPipe 'F' 'S' 'S' = True
checkPipe 'F' '-' 'E' = True
checkPipe 'F' 'J' 'E' = True
checkPipe 'F' '7' 'E' = True
checkPipe 'F' 'S' 'E' = True
checkPipe 'S' '|' 'N' = True
checkPipe 'S' '7' 'N' = True
checkPipe 'S' 'F' 'N' = True
checkPipe 'S' '-' 'E' = True
checkPipe 'S' 'J' 'E' = True
checkPipe 'S' '7' 'E' = True
checkPipe 'S' '|' 'S' = True
checkPipe 'S' 'L' 'S' = True
checkPipe 'S' 'J' 'S' = True
checkPipe 'S' '-' 'W' = True
checkPipe 'S' 'L' 'W' = True
checkPipe 'S' 'F' 'W' = True
checkPipe _ _ _ = False

replaceS :: [String] -> (Int, Int) -> Char
replaceS input (x, y)
    | dirs == "NE" = 'J'
    | dirs == "NW" = 'L'
    | dirs == "ES" = '7'
    | dirs == "SW" = 'F'
    | dirs == "NS" = '|'
    | dirs == "EW" = '-'
    | otherwise = error "Invalid pipe"
  where
    dirs = map deltaToDir $ pipeNeighbors input (x, y)

deltaToDir :: (Int, Int) -> Char
deltaToDir (0, -1) = 'N'
deltaToDir (1, 0) = 'E'
deltaToDir (0, 1) = 'S'
deltaToDir (-1, 0) = 'W'
deltaToDir _ = 'X'

at :: [String] -> (Int, Int) -> Char
at input (x, y) = (input !! y) !! x

replaceAt :: [String] -> (Int, Int) -> Char -> [String]
replaceAt input (x, y) c = take y input ++ [take x row ++ [c] ++ drop (x + 1) row] ++ drop (y + 1) input
  where
    row = input !! y

validDirs :: [String] -> (Int, Int) -> [(Int, Int)]
validDirs input (x, y) = filter check dirs
  where
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    check (dx, dy) = x + dx >= 0 && x + dx < length (head input) && y + dy >= 0 && y + dy < length input

pipeNeighbors :: [String] -> (Int, Int) -> [(Int, Int)]
pipeNeighbors input (x, y) = filter check $ validDirs input (x, y)
  where
    check (dx, dy) = checkPipe (at input (x, y)) (at input (x + dx, y + dy)) (deltaToDir (dx, dy))

bfs :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
bfs _ [] acc = acc
bfs input ((x, y) : queue) acc
    | (x, y) `elem` acc = bfs input queue acc
    | otherwise = bfs input queue' acc'
  where
    neighbors = map add $ pipeNeighbors input (x, y)
    queue' = queue ++ neighbors
    acc' = (x, y) : acc
    add (dx, dy) = (x + dx, y + dy)

mkCrosses :: [String] -> [(Int, Int)] -> (Int, Int) -> M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
mkCrosses m poly (w, h) acc (sx, sy) = M.insert (sx, sy) direction acc
  where
    points = zip [sx .. w] [sy .. h]
    before = length $ filter (not . corner . at m) $ filter (`elem` poly) $ takeWhile (`M.notMember` acc) points
    after = case dropWhile (`M.notMember` acc) points of
        [] -> 0
        p : _ -> acc M.! p
    direction = before + after
    corner c = c == 'L' || c == '7'

intersections :: M.Map (Int, Int) Int -> Int
intersections = M.size . M.filter odd

part1 :: String -> String
part1 input = show $ (`div` 2) $ length $ bfs m [(sx, sy)] []
  where
    m = lines input
    (sx, sy) = findS m

part2 :: String -> String
part2 input = show $ intersections inner
  where
    m = lines input
    (sx, sy) = findS m
    m' = replaceAt m (sx, sy) $ replaceS m (sx, sy)
    poly = bfs m' [(sx, sy)] []
    xMin = minimum $ map fst poly
    xMax = maximum $ map fst poly
    yMin = minimum $ map snd poly
    yMax = maximum $ map snd poly
    points = reverse [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax], (x, y) `notElem` poly]
    inner = foldl (mkCrosses m' poly (xMax, yMax)) M.empty points

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
