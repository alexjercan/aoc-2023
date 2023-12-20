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

parents :: Name -> [Module] -> [Module]
parents n = filter (elem n . destinations)

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
        go (Conjunction n _) = ConjunctionState (M.fromList $ map ((,Low) . name) $ parents n modules)

type MachineState = State (M.Map Name (Module, ModuleState))

applyOneM :: (Name, Name, Signal) -> MachineState [(Name, Name, Signal)]
applyOneM (n, n', s) = do
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

applyM :: [(Name, Name, Signal)]  -> MachineState [(Name, Name, Signal)]
applyM xs = concat <$> mapM applyOneM xs

buttonM :: MachineState (Int, Int)
buttonM = go [("broadcaster","button", Low)]
    where
        go [] = return (0, 0)
        go xs = do
            (l, h) <- applyM xs >>= go
            let l' = length $ filter (\(_, _, s) -> s == Low) xs
            let h' = length $ filter (\(_, _, s) -> s == High) xs
            return (l + l', h + h')

buttonNM :: Int -> MachineState Int
buttonNM n = go n 0 0
    where
        go 0 l h = return $ l * h
        go i l h = do
            (l', h') <- buttonM
            go (i - 1) (l + l') (h + h')

simulate :: Int -> [Module] -> Int
simulate n ms = evalState (buttonNM n) (initialState ms)

part1 :: String -> String
part1 = show . simulate 1000 . parse modulesP

buttonStateM :: Name -> Signal -> MachineState Bool
buttonStateM n s = go [("broadcaster","button", Low)]
    where
        go [] = return False
        go xs = if any (\(n', _, s') -> n == n' && s == s') xs
                    then return True
                    else applyM xs >>= go

buttonStateNM :: Name -> Signal -> MachineState Int
buttonStateNM n s = go 1
    where
        go c = buttonStateM n s >>= \case
                True -> return c
                False -> go (c + 1)

simulate' :: Name -> Signal -> [Module] -> Int
simulate' n s ms = evalState (buttonStateNM n s) (initialState ms)

solution :: Name -> Signal -> [Module] -> Int
solution n s ms = minimum $ map (\n' -> foldl1 lcm $ map ((\n'' -> simulate' n'' s ms) . name) (parents n' ms)) ps
    where
        ps = map name $ parents n ms

part2 :: String -> String
part2 = show . solution "rx" Low . parse modulesP

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
