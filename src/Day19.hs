module Day19 (main, part1, part2) where

import qualified Data.Map as M
import qualified Text.Parsec as P
import Util.Parser (Parser, parse)

data Category = X | M | A | S deriving (Show, Eq)

data Condition = ConditionL Category Int | ConditionG Category Int deriving (Show, Eq)

data Name = Accepted | Rejected | Name String deriving (Show, Eq, Ord)

data Rule = Rule Condition Name deriving (Show, Eq)

data Workflow = Workflow Name [Rule] Name deriving (Show, Eq)

data Part = Part Int Int Int Int deriving (Show, Eq)

data PartRange = PartRange (Int, Int) (Int, Int) (Int, Int) (Int, Int) deriving (Show, Eq)

categoryP :: Parser Category
categoryP = P.choice [P.char 'x' >> return X, P.char 'm' >> return M, P.char 'a' >> return A, P.char 's' >> return S]

conditionP :: Parser Condition
conditionP = do
    c <- categoryP
    P.choice
        [ P.char '<' >> ConditionL c <$> (read <$> P.many1 P.digit)
        , P.char '>' >> ConditionG c <$> (read <$> P.many1 P.digit)
        ]

nameP :: Parser Name
nameP =
    P.choice
        [ P.string "A" >> return Accepted
        , P.string "R" >> return Rejected
        , Name <$> P.many1 P.letter
        ]

ruleP :: Parser Rule
ruleP = do
    c <- conditionP <* P.char ':'
    Rule c <$> nameP

workflowP :: Parser Workflow
workflowP = do
    name <- nameP
    (rs, def) <- P.between (P.char '{') (P.char '}') ((,) <$> P.sepEndBy (P.try ruleP) (P.char ',') <*> nameP)
    return $ Workflow name rs def

workflowsP :: Parser [Workflow]
workflowsP = P.sepEndBy workflowP (P.char '\n')

ratingP :: Parser Int
ratingP = do
    _ <- P.anyChar <* P.char '='
    read <$> P.many1 P.digit

partP :: Parser Part
partP = do
    nums <- P.between (P.char '{') (P.char '}') (P.sepBy ratingP (P.char ','))
    case nums of
        [a, b, c, d] -> return $ Part a b c d
        _ -> fail "Invalid part"

partsP :: Parser [Part]
partsP = P.sepEndBy partP (P.char '\n')

inputP :: Parser (M.Map Name Workflow, [Part])
inputP = do
    ws <- workflowsP <* P.char '\n'
    let ws' = M.fromList $ map (\w@(Workflow n _ _) -> (n, w)) ws
    ps <- partsP <* P.eof
    return (ws', ps)

category :: Category -> Part -> Int
category X (Part x _ _ _) = x
category M (Part _ m _ _) = m
category A (Part _ _ a _) = a
category S (Part _ _ _ s) = s

condition :: Condition -> Part -> Bool
condition (ConditionL c i) p = category c p < i
condition (ConditionG c i) p = category c p > i

rules :: [Rule] -> Part -> Name -> Name
rules [] _ def = def
rules (Rule c n : rs) p def = if condition c p then n else rules rs p def

workflow :: M.Map Name Workflow -> Part -> Name -> Name
workflow ws p n = case M.lookup n ws of
    Just (Workflow _ rs def) -> rules rs p def
    Nothing -> error "No workflow found"

simulate1 :: M.Map Name Workflow -> Part -> Int
simulate1 ws p@(Part x m a s) = case until (\n -> n == Accepted || n == Rejected) (workflow ws p) (Name "in") of
    Accepted -> x + m + a + s
    Rejected -> 0
    _ -> error "unreachable"

simulate :: M.Map Name Workflow -> [Part] -> Int
simulate ws = sum . map (simulate1 ws)

part1 :: String -> String
part1 = show . uncurry simulate . parse inputP

conditionRange :: Condition -> PartRange -> (Maybe PartRange, Maybe PartRange)
conditionRange (ConditionL X x) (PartRange (x1, x2) m a s)
    | x <= x1 = (Nothing, Just $ PartRange (x1, x2) m a s)
    | x > x2 = (Just $ PartRange (x1, x2) m a s, Nothing)
    | otherwise = (Just $ PartRange (x1, x - 1) m a s, Just $ PartRange (x, x2) m a s)
conditionRange (ConditionL M m) (PartRange x (m1, m2) a s)
    | m <= m1 = (Nothing, Just $ PartRange x (m1, m2) a s)
    | m > m2 = (Just $ PartRange x (m1, m2) a s, Nothing)
    | otherwise = (Just $ PartRange x (m1, m - 1) a s, Just $ PartRange x (m, m2) a s)
conditionRange (ConditionL A a) (PartRange x m (a1, a2) s)
    | a <= a1 = (Nothing, Just $ PartRange x m (a1, a2) s)
    | a > a2 = (Just $ PartRange x m (a1, a2) s, Nothing)
    | otherwise = (Just $ PartRange x m (a1, a - 1) s, Just $ PartRange x m (a, a2) s)
conditionRange (ConditionL S s) (PartRange x m a (s1, s2))
    | s <= s1 = (Nothing, Just $ PartRange x m a (s1, s2))
    | s > s2 = (Just $ PartRange x m a (s1, s2), Nothing)
    | otherwise = (Just $ PartRange x m a (s1, s - 1), Just $ PartRange x m a (s, s2))
conditionRange (ConditionG X x) (PartRange (x1, x2) m a s)
    | x < x1 = (Just $ PartRange (x1, x2) m a s, Nothing)
    | x >= x2 = (Nothing, Just $ PartRange (x1, x2) m a s)
    | otherwise = (Just $ PartRange (x + 1, x2) m a s, Just $ PartRange (x1, x) m a s)
conditionRange (ConditionG M m) (PartRange x (m1, m2) a s)
    | m < m1 = (Just $ PartRange x (m1, m2) a s, Nothing)
    | m >= m2 = (Nothing, Just $ PartRange x (m1, m2) a s)
    | otherwise = (Just $ PartRange x (m + 1, m2) a s, Just $ PartRange x (m1, m) a s)
conditionRange (ConditionG A a) (PartRange x m (a1, a2) s)
    | a < a1 = (Just $ PartRange x m (a1, a2) s, Nothing)
    | a >= a2 = (Nothing, Just $ PartRange x m (a1, a2) s)
    | otherwise = (Just $ PartRange x m (a + 1, a2) s, Just $ PartRange x m (a1, a) s)
conditionRange (ConditionG S s) (PartRange x m a (s1, s2))
    | s < s1 = (Just $ PartRange x m a (s1, s2), Nothing)
    | s >= s2 = (Nothing, Just $ PartRange x m a (s1, s2))
    | otherwise = (Just $ PartRange x m a (s + 1, s2), Just $ PartRange x m a (s1, s))

rulesRange :: [Rule] -> Maybe PartRange -> Name -> [(PartRange, Name)]
rulesRange _ Nothing _ = []
rulesRange [] (Just r) def = [(r, def)]
rulesRange (Rule c n : rs) (Just r) def = case conditionRange c r of
    (Nothing, Just r') -> rulesRange rs (Just r') def
    (Just r', Nothing) -> [(r', n)]
    (Just r1, Just r2) -> (r1, n) : rulesRange rs (Just r2) def
    _ -> error "unreachable"

workflowRange' :: M.Map Name Workflow -> PartRange -> Name -> [(PartRange, Name)]
workflowRange' _ r Accepted = [(r, Accepted)]
workflowRange' _ r Rejected = [(r, Rejected)]
workflowRange' ws p n = case M.lookup n ws of
    Just (Workflow _ rs def) -> rulesRange rs (Just p) def
    Nothing -> error "No workflow found"

workflowRange :: M.Map Name Workflow -> [(PartRange, Name)] -> [(PartRange, Name)]
workflowRange ws = concatMap (uncurry (workflowRange' ws))

simulate1' :: M.Map Name Workflow -> PartRange -> Int
simulate1' ws p =
    let rns = until (all (\(_, n) -> n == Accepted || n == Rejected)) (workflowRange ws) [(p, Name "in")]
        rns' = filter (\(_, n) -> n == Accepted) rns
        rs = map fst rns'
     in sum $ map (\(PartRange (x1, x2) (m1, m2) (a1, a2) (s1, s2)) -> (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)) rs

part2 :: String -> String
part2 = show . uncurry simulate1' . (\(ws, _) -> (ws, PartRange (1, 4000) (1, 4000) (1, 4000) (1, 4000))) . parse inputP

solve :: String -> String
solve input = "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input ++ "\n"

main :: IO ()
main = interact solve
