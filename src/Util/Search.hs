module Util.Search (dijkstra) where

import Control.Monad.State (
    MonadState (get, put),
    State,
    evalState,
    forM_,
    modify,
 )
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.PSQueue as P
import qualified Data.Set as S

data DijkstraState a t
    = DijkstraState (S.Set a) (P.PSQ a t) (M.Map a t) (M.Map a a)

minimumVertex :: P.PSQ a t -> a
minimumVertex = P.key . fromJust . P.findMin

ltJust :: (Ord t) => t -> Maybe t -> Bool
ltJust _ Nothing = True
ltJust alt (Just c) = alt < c

handleNeighbor ::
    (Ord a, Ord t, Num t) =>
    -- | Source Node
    a ->
    -- | Neighbor and cost
    (t, a) ->
    -- | Original state
    DijkstraState a t ->
    DijkstraState a t
handleNeighbor u (cost, v) (DijkstraState s q dist prev) = do
    let alt = dist M.! u + cost
    if alt `ltJust` P.lookup v q
        then
            DijkstraState
                (S.delete v s)
                (P.insert v alt q)
                (M.insert v alt dist)
                (M.insert v u prev)
        else DijkstraState s q dist prev

dijkstraM ::
    (Ord a, Ord t, Num t) =>
    -- | Check if node is target
    (a -> Bool) ->
    -- | Generate neighbors for a node and the cost
    (a -> [(t, a)]) ->
    -- | Generate answer based on the final scores and path
    (M.Map a t -> M.Map a a -> b) ->
    State (DijkstraState a t) b
dijkstraM isTarget getNeighbors answerF = do
    (DijkstraState s q dist prev) <- get
    if P.null q
        then return $ answerF dist prev
        else do
            let u = minimumVertex q
            let q' = P.delete u q
            let s' = S.insert u s
            put (DijkstraState s' q' dist prev)
            if isTarget u
                then return $ answerF dist prev
                else do
                    forM_
                        (filter (not . (`S.member` s') . snd) (getNeighbors u))
                        (modify . handleNeighbor u)
                    dijkstraM isTarget getNeighbors answerF

dijkstra ::
    (Ord a, Ord t, Num t) =>
    -- | Source node
    a ->
    -- | Target node
    (a -> Bool) ->
    -- | Generate neighbors and costs
    (a -> [(t, a)]) ->
    -- | Generate answer based on the final scores and path
    (M.Map a t -> M.Map a a -> b) ->
    b
dijkstra source targetP getNeighbors answerF =
    evalState (dijkstraM targetP getNeighbors answerF) s
  where
    s =
        DijkstraState
            S.empty
            (P.singleton source 0)
            (M.singleton source 0)
            M.empty
