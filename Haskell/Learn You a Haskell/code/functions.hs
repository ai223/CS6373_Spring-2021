
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER 7"
lucky x = "Sorry, you're out of luck, pal."


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a


badGreeting :: String
badGreeting = "Oh! Pfft. It's you,"

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name ++ "."
