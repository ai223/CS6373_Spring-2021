-- Q1 (Write a program that finds the maximum of a simple list of numbers.)

maxm :: (Ord a) => [a] -> a
maxm [] = error "max of empty list"
maxm [x] = x
maxm (x:xs) = maxmHelper xs x

maxmHelper :: (Ord a) => [a] -> a -> a
maxmHelper [] y = y
maxmHelper (x:xs) y =
  case compare x y of
    LT -> maxmHelper xs y
    EQ -> maxmHelper xs x
    GT -> maxmHelper xs x

-- Q2 (Write a program that succeeds if the intersection of two given list parameters is empty.)

noIntersection :: Ord a => [a] -> [a] -> Bool
noIntersection xs ys = noIntersectionHelper (sort' xs) (sort' ys)

noIntersectionHelper :: Ord a => [a] -> [a] -> Bool
noIntersectionHelper [] _ = True
noIntersectionHelper _ [] = True
noIntersectionHelper list1@(x:xs) list2@(y:ys)
  | y > x = noIntersectionHelper xs list2
  | x > y = noIntersectionHelper list1 ys
  | otherwise = False

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = (sort' smaller) ++ [x] ++ (sort' larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]

-- Q3 (Write a program that returns a list containing the union of the elements of two given lists.
-- Assume the list represents sets (i.e. unique elements) and the union returns unique elements.)

union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] ys = ys
union xs [] = xs
union xs ys = unionHelper (sort' xs) (sort' ys) []

unionHelper :: Ord a => [a] -> [a] -> [a] -> [a]
unionHelper xs [] zs = merge xs zs
unionHelper [] ys zs = merge ys zs
unionHelper (x:xs) (y:ys) zs =
  case compare x y of
    LT -> unionHelper xs (y:ys) (x:zs)
    EQ -> unionHelper xs ys (x:zs)
    GT -> unionHelper (x:xs) ys (y:zs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Q4 (Write a program that returns the final element of a list)

lst :: [a] -> a
lst [] = error "Cannot return last element of empty list"
lst [x] = x
lst [x1, x2] = x2
lst (x1:x2:xs) = lst xs
