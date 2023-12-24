module Day24 (main, part1, part2) where

import Util.Parser (Parser, parse)
import qualified Text.Parsec as P

type Vec3 = (Integer, Integer, Integer)
type Particle = (Vec3, Vec3)

negP :: Parser Integer
negP = negate . read <$> (P.char '-' *> P.many1 P.digit)

posP :: Parser Integer
posP = read <$> P.many1 P.digit

numP :: Parser Integer
numP = negP P.<|> posP

vec3P :: Parser Vec3
vec3P = do
  x <- numP <* P.char ',' <* P.spaces
  y <- numP <* P.char ',' <* P.spaces
  z <- numP
  return (x, y, z)

particleP :: Parser Particle
particleP = do
  p <- vec3P <* P.spaces <* P.char '@' <* P.spaces
  v <- vec3P
  return (p, v)

inputP :: Parser [Particle]
inputP = P.many1 (particleP <* P.spaces) <* P.eof

intersect :: Particle -> Particle -> Bool
intersect ((x1, y1, _), (dx1, dy1, _)) ((x2, y2, _), (dx2, dy2, _)) =
    let xdiff = (-dx1, -dx2)
        ydiff = (-dy1, -dy2)
        det (a, b) = fst a * snd b - snd a * fst b
        div' = det (xdiff, ydiff)
        d = (det ((x1, y1), (x1 + dx1, y1 + dy1)), det ((x2, y2), (x2 + dx2, y2 + dy2)))
        x = det (d, xdiff) `div` div'
        y = det (d, ydiff) `div` div'
    in  div' /= 0
        && 200000000000000 <= x && x <= 400000000000000 && 200000000000000 <= y && y <= 400000000000000
        && (x - x1 > 0) == (dx1 > 0) && (y - y1 > 0) == (dy1 > 0)
        && (x - x2 > 0) == (dx2 > 0) && (y - y2 > 0) == (dy2 > 0)

combinations2 :: [a] -> [(a, a)]
combinations2 [] = []
combinations2 (x:xs) = map ((,) x) xs ++ combinations2 xs

solution :: [Particle] -> Int
solution particles = length $ filter (uncurry intersect) $ combinations2 particles

part1 :: String -> String
part1 = show . solution . parse inputP

part2 :: String -> String
part2 = const ""

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
