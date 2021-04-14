
sumpairs :: (Num a, Ord a) => [a] -> a -> [(a, a)]
sumpairs [] _ = []
sumpairs xs x = sumpairsHelper (sort' xs) (reverse' (sort' xs)) x []

sumpairsHelper :: (Num a, Ord a) => [a] -> [a] -> a -> [(a, a)] -> [(a, a)]
sumpairsHelper [] [] z ps = ps
sumpairsHelper (x:xs) (y:ys) z ps
  | y < x = ps
  | otherwise =
      case compare (x + y) z of
        LT -> sumpairsHelper xs (y:ys) z ps
        EQ -> sumpairsHelper xs ys z (ps ++ [(x,y)])
        GT -> sumpairsHelper (x:xs) ys z ps

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = (sort' smaller) ++ [x] ++ (sort' larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
