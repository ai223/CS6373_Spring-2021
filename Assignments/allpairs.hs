import Data.Map (Map)
import qualified Data.Map as Map

findAllPairs :: (Num a, Ord a) => [a] -> Map a [(a,a)]
findAllPairs [] = Map.empty
findAllPairs xs = findAllPairsHelper xs [] Map.empty

findAllPairsHelper :: (Num a, Ord a) => [a] -> [a] -> Map a [(a,a)] -> Map a [(a,a)]
findAllPairsHelper [] _ m = m
findAllPairsHelper (x:xs) ys m = findAllPairsHelper xs (x:ys) (sumAllPairs x ys m)

sumAllPairs :: (Num a, Ord a) => a -> [a] -> Map a [(a,a)] -> Map a [(a,a)]
sumAllPairs x [] m = m
sumAllPairs x (y:ys) m = do
  let add = x + y
  let pairs = Map.findWithDefault [] add m
  let newPairs = (x,y):pairs
  sumAllPairs x ys (Map.insert add newPairs m)
