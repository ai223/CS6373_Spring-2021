reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 xs = reverseHelper xs []

reverseHelper :: [a] -> [a] -> [a]
reverseHelper [] ys = ys
reverseHelper (x:xs) ys = reverseHelper xs (x:ys)
