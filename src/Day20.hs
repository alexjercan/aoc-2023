{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Day20 (main, part1, part2) where

import Util.Parser (Parser, parse)
import qualified Text.Parsec as P
import qualified Data.Map as M
import Control.Monad.State (State, gets, modify, evalState)

type Name = String
data Module
    = Broadcaster [Name]
    | FlipFlop Name [Name]
    | Conjunction Name [Name]
    deriving (Show, Eq)

name :: Module -> Name
name (Broadcaster _) = "broadcaster"
name (FlipFlop n _) = n
name (Conjunction n _) = n

destinations :: Module -> [Name]
destinations (Broadcaster ns) = ns
destinations (FlipFlop _ ns) = ns
destinations (Conjunction _ ns) = ns

data Signal = Low | High deriving (Show, Eq)
data Status = Off | On deriving (Show, Eq)
data ModuleState
    = BroadcasterState
    | FlipFlopState Status
    | ConjunctionState (M.Map Name Signal)
    deriving (Show, Eq)

destinationsP :: Parser [Name]
destinationsP = P.sepBy1 (P.many1 P.letter) (P.string "," <* P.spaces)

broadcasterP :: Parser Module
broadcasterP = do
    _ <- P.string "broadcaster" <* P.spaces <* P.string "->" <* P.spaces
    Broadcaster <$> destinationsP

flipFlopP :: Parser Module
flipFlopP = do
    _ <- P.char '%'
    n <- P.many1 P.letter <* P.spaces <* P.string "->" <* P.spaces
    FlipFlop n <$> destinationsP

conjunctionP :: Parser Module
conjunctionP = do
    _ <- P.char '&'
    n <- P.many1 P.letter <* P.spaces <* P.string "->" <* P.spaces
    Conjunction n <$> destinationsP

moduleP :: Parser Module
moduleP = broadcasterP P.<|> flipFlopP P.<|> conjunctionP

modulesP :: Parser [Module]
modulesP = P.many1 (moduleP <* P.spaces) <* P.eof

initialState :: [Module] -> M.Map Name (Module, ModuleState)
initialState modules = M.fromList $ map (\m -> (name m, (m, go m))) modules
    where
        go Broadcaster{} = BroadcasterState
        go (FlipFlop _ _) = FlipFlopState Off
        go (Conjunction n _) = ConjunctionState (M.fromList $ map ((,Low) . name) $ filter (elem n . destinations) modules)

type MachineState = State (M.Map Name (Module, ModuleState))

updateState :: Name -> Name -> Signal -> MachineState [(Name, Name, Signal)]
updateState n n' s = do
    gets (M.lookup n) >>= \case
        Nothing -> return []
        Just (Broadcaster ns, BroadcasterState) -> return $ map (,n,s) ns
        Just (FlipFlop _ ns, FlipFlopState st) -> do
            case (s, st) of
                (High, _) -> return []
                (Low, Off) -> do
                    modify $ M.insert n (FlipFlop n ns, FlipFlopState On)
                    return $ map (,n,High) ns
                (Low, On) -> do
                    modify $ M.insert n (FlipFlop n ns, FlipFlopState Off)
                    return $ map (,n,Low) ns
        Just (Conjunction _ ns, ConjunctionState m) -> do
            let m' = M.insert n' s m
            modify $ M.insert n (Conjunction n ns, ConjunctionState m')
            return $ map (,n,if all (== High) $ M.elems m' then Low else High) ns
        _ -> return []

apply :: [(Name, Name, Signal)]  -> MachineState [(Name, Name, Signal)]
apply xs = concat <$> mapM (\(n, n', s) -> updateState n n' s) xs

buttonM :: MachineState ([(Name, Name, Signal)], (Int, Int))
buttonM = go [("broadcaster","button", Low)]
    where
        go [] = return ([], (0, 0))
        go xs = do
            xs' <- apply xs
            (xs'', (l, h)) <- go xs'
            return (xs ++ xs'', (l + length (filter (\(_, _, s) -> s == Low) xs), h + length (filter (\(_, _, s) -> s == High) xs)))

simulate :: Int -> [Module] -> (Int, Int)
simulate n ms = snd $ evalState (go n) (initialState ms)
    where
        go 0 = return ([], (0, 0))
        go i = do
            (xs, (l, h)) <- buttonM
            (xs', (l', h')) <- go (i - 1)
            return (xs ++ xs', (l + l', h + h'))

part1 :: String -> String
part1 = show . simulate 1000 . parse modulesP

part2 :: String -> String
part2 = const ""

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
