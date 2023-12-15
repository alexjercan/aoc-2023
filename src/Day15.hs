module Day15 (main, part1, part2) where

import Data.Char (ord)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Text.Parsec as P
import Util.Parser (Parser, parse)

data Operation = Insert String Int | Delete String deriving (Show)

csvP :: Parser [String]
csvP = P.sepBy (P.many1 $ P.noneOf ",\n") (P.char ',') <* P.spaces <* P.eof

insertP :: String -> Parser Operation
insertP s = do
    _ <- P.char '='
    n <- P.many1 P.digit
    return $ Insert s (read n)

deleteP :: String -> Parser Operation
deleteP s = do
    _ <- P.char '-'
    return $ Delete s

operationP :: Parser Operation
operationP = do
    s <- P.many1 P.letter
    insertP s P.<|> deleteP s

operationsP :: Parser [Operation]
operationsP = P.sepBy operationP (P.char ',') <* P.spaces <* P.eof

hash :: String -> Int
hash = foldl (\acc c -> (acc + c) * 17 `mod` 256) 0 . map ord

part1 :: String -> String
part1 = show . sum . map hash . parse csvP

insertPair :: (String, Int) -> [(String, Int)] -> [(String, Int)]
insertPair (s, n) [] = [(s, n)]
insertPair (s, n) ((s', n') : xs)
    | s == s' = (s, n) : xs
    | otherwise = (s', n') : insertPair (s, n) xs

operation :: V.Vector [(String, Int)] -> Operation -> V.Vector [(String, Int)]
operation v (Insert s n) = V.modify (\v' -> MV.modify v' (insertPair (s, n)) (hash s)) v
operation v (Delete s) = V.modify (\v' -> MV.modify v' (filter (\(s', _) -> s /= s')) (hash s)) v

simulate :: [Operation] -> V.Vector [(String, Int)]
simulate = foldl operation (V.replicate 256 [])

power :: V.Vector [(String, Int)] -> Int
power = V.sum . V.imap (\i -> sum . zipWith (\j (_, x) -> (i + 1) * j * x) [1 ..])

input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

part2 :: String -> String
part2 = show . power . simulate . parse operationsP

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
