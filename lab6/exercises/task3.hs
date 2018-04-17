module Task3 where

import Control.Monad.Trans.List
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Graph

containsAny :: Ord a => [a] -> [a] -> Bool
a `containsAny` b = foldl (||) False $ map (`elem` a) b

isIndependent :: Graph -> [Vertex] -> Vertex -> Bool
isIndependent g independent vertex = not $ (reachable g vertex) `containsAny` independent

step :: Graph -> [Vertex] -> [Vertex] -> [[Vertex]]
step g independent [] = [independent]
step g independent (v:vs) =
  if isIndependent g independent v then
    step g (v:independent) vs ++ step g independent vs
  else
    step g independent vs

someGraph :: Graph
someGraph = buildG (1, 5) [(1, 2), (2, 1), (3, 4), (4,3)]

findMaxIndependentVertexSet :: Graph -> [Vertex]
findMaxIndependentVertexSet g = fst $ foldl largerVertexSet ([], 0) $ map listWithLength independentVertexSets
    where
      listWithLength :: [a] -> ([a], Int)
      listWithLength l = (l, length l)

      independentVertexSets :: [[Vertex]]
      independentVertexSets = step g [] (vertices g)

      largerVertexSet :: ([a], Int) -> ([a], Int) -> ([a], Int)
      largerVertexSet s1@(_, l1) s2@(_, l2) = if l1 > l2
        then
          s1
        else
          s2


