maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum xs)


-- function defintion
replicate' :: (Int) -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x - 1) y

-- guards
replicate'' :: (Int) -> a -> [a]
replicate'' n x
    | n <= 0 = []
    | otherwise = x : replicate'' (n - 1) x


take' :: (Int) -> [a] -> [a]
take' n xs
  | (n <= 0) || (null xs) = []
  | otherwise = head xs : take' (n - 1) (tail xs)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' x = x : repeat' x


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- recursive helper function
tuple :: a -> [b] -> [(a, b)]
tuple _ [] = []
tuple a (x:xs) = (a,x) : tuple a xs

-- main function
-- permute :: [a] -> [b] -> [(a, b)]
-- permute [] _ = []
-- permute _ [] = []
-- permute (x:xs) ys = tuple x ys ++ permute xs ys

permute :: [a] -> [b] -> [(a, b)]
permute [] _ = []
permute _ [] = []
permute (x:xs) (ys) = (x, head ys) : permute [x] (tail ys) ++ permute xs ys
