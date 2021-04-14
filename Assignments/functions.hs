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

intersects :: Ord a => [a] -> [a] -> Bool
intersects xs ys = intersectsHelper (sort' xs) (sort' ys)

intersectsHelper :: Ord a => [a] -> [a] -> Bool
intersectsHelper [] _ = False
intersectsHelper _ [] = False
intersectsHelper list1@(x:xs) list2@(y:ys)
  | y > x = intersectsHelper xs list2
  | x > y = intersectsHelper list1 ys
  | otherwise = True

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = (sort' smaller) ++ [x] ++ (sort' larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]

-- Q3 (Write a program that returns a list containing the union of the elements of two given lists.
-- Assume the list represents sets (i.e. unique elements) and the union returns unique elements.)

-- intersection :: Ord a => [a] -> [a] -> [a]
-- intersection [] _ = []
-- intersection _ [] = []
-- intersection xs ys = intersectionHelper (sort' xs) (sort' ys) []
--
-- intersectionHelper :: Ord a => [a] -> [a] -> [a] -> [a]
-- intersectionHelper [] ys zs = zs
-- intersectionHelper xs [] zs = zs
-- intersectionHelper list1@(x:xs) list2@(y:ys) zs =
--   case compare x y of
--     LT -> unionHelper xs list2 zs
--     EQ -> unionHelper xs ys (x:zs)
--     GT -> unionHelper list1 ys zs

-- Q4 (Write a program that returns the final element of a list)

lst :: [a] -> a
lst [] = error "Cannot return last element of empty list"
lst [x] = x
lst [x1, x2] = x2
lst (x1:x2:xs) = lst xs
