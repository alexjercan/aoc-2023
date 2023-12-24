module Day24 (main, part1, part2) where

import Util.Parser (Parser, parse)
import qualified Text.Parsec as P

type Vec2 = (Double, Double)
type Vec3 = (Double, Double, Double)
type Particle2D = (Vec2, Vec2)
type Particle3D = (Vec3, Vec3)

dropZ :: Particle3D -> Particle2D
dropZ ((x, y, _), (dx, dy, _)) = ((x, y), (dx, dy))

negP :: Parser Double
negP = negate . read <$> (P.char '-' *> P.many1 P.digit)

posP :: Parser Double
posP = read <$> P.many1 P.digit

numP :: Parser Double
numP = negP P.<|> posP

vec3P :: Parser Vec3
vec3P = do
  x <- numP <* P.char ',' <* P.spaces
  y <- numP <* P.char ',' <* P.spaces
  z <- numP
  return (x, y, z)

particleP :: Parser Particle3D
particleP = do
  p <- vec3P <* P.spaces <* P.char '@' <* P.spaces
  v <- vec3P
  return (p, v)

inputP :: Parser [Particle3D]
inputP = P.many1 (particleP <* P.spaces) <* P.eof

border :: (Num a, Ord a) => a -> Bool
border x = 200000000000000 <= x && x <= 400000000000000

det :: Vec2 -> Vec2 -> Double
det (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

intersect2D :: Particle2D -> Particle2D -> Bool
intersect2D ((x1, y1), (dx1, dy1)) ((x2, y2), (dx2, dy2)) =
    let xd = (-dx1, -dx2)
        yd = (-dy1, -dy2)
        dv = det xd yd
        d = (det (x1, y1) (x1 + dx1, y1 + dy1), det (x2, y2) (x2 + dx2, y2 + dy2))
        x = det d xd / dv
        y = det d yd / dv
    in
        dv /= 0 &&
        (x - x1 > 0) == (dx1 > 0) && (y - y1 > 0) == (dy1 > 0) &&
        (x - x2 > 0) == (dx2 > 0) && (y - y2 > 0) == (dy2 > 0) &&
        border x && border y

solution :: [Particle2D] -> Int
solution particles = length $ filter (uncurry intersect2D) [(p1, p2) | p1 <- particles, p2 <- particles, p1 /= p2]

part1 :: String -> String
part1 = show . solution . map dropZ . parse inputP

part2 :: String -> String
part2 = const ""

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
