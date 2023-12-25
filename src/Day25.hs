module Day25 (main, part1) where

import qualified Data.Map as M
import Util.Parser (Parser, parse)
import qualified Text.Parsec as P
import Control.Monad.State (
    MonadState (get, put),
    State,
    evalState,
    modify,
 )
import System.Random
import Data.List (minimumBy)
import Control.Monad (replicateM)
import Data.Function (on)

pairP :: Parser (String, [String])
pairP = do
    node <- P.many1 P.letter <* P.char ':' <* P.spaces
    children <- P.sepBy1 (P.many1 P.letter) (P.char ' ') <* P.spaces
    return (node, children)

inputP :: Parser (M.Map String [String])
inputP = do
    pairs <- P.many1 pairP
    let g = M.fromList pairs
    return $ foldl (\g' (n, ns) -> foldl (\g'' n' -> M.insertWith (++) n' [n] g'') g' ns) g pairs

data KargerState = KargerState (StdGen)

choice :: StdGen -> [a] -> (a, StdGen)
choice g [] = error "choice: empty list"
choice g xs = (xs !! i, g')
    where (i, g') = randomR (0, length xs - 1) g

contract :: String -> String -> (M.Map String [String], M.Map String [String]) -> (M.Map String [String], M.Map String [String])
contract u v (g, s) =
    let g' = M.delete v $ M.delete u g
        g'' = M.insert u (g M.! u ++ g M.! v) g'
        g''' = M.map (\xs -> map (\x -> if x == v then u else x) xs) g''
        g'''' = M.update (\xs -> Just $ filter (/= u) xs) u g'''
        s' = M.delete v $ M.delete u s
        s'' = M.insert u (s M.! u ++ s M.! v) s'
    in (g'''', s'')

minCutM :: M.Map String [String] -> M.Map String [String] -> State KargerState (Int, M.Map String [String])
minCutM graph s = do
    KargerState g <- get
    if M.size graph <= 2
        then return $ (length $ head $ M.elems graph, s)
        else do
            let keys = M.keys graph
            let (u, g') = choice g keys
            let (v, g'') = choice g' $ filter (\x -> x `M.member` graph)  $ graph M.! u
            let (graph', s') = contract u v (graph, s)
            put $ KargerState g''
            minCutM graph' s'

karger :: M.Map String [String] -> (Int, M.Map String [String])
karger graph = head $ dropWhile (\(c, _) -> c > 3) $ evalState (replicateM ((length graph) * (length graph)) (minCutM graph s)) (KargerState $ mkStdGen 0)
    where s = M.fromList $ zip (M.keys graph) (map (:[]) $ M.keys graph)

solution :: M.Map String [String] -> String
solution graph = show $ M.foldl (*) 1 $ M.map length $ snd $ karger graph

part1 :: String -> String
part1 = solution . parse inputP

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\n"

main :: IO ()
main = interact solve
