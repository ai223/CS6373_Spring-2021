-- new types are defined using the 'data' keyword

-- we are redefining the bool value; '|' means 'or'
-- Bool' can be either "True" or "False"
data Bool' = False | True

data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
             deriving (Show)

-- The Circle value constructor has three fields, which takes floats
-- the Rectangle value construcor accepts 4 fields that are all floats
-- Value constructors are functions that return a value of a data type

-- Circle is NOT a type, only Shape is
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


data Point = Point Float Float deriving(Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ r) = pi * r ^ 2
area' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x + a) (y + b)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b =
  Rectangle' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle' :: Float -> Shape'
baseCircle' r = Circle' (Point 0 0) r

baseRect' :: Float -> Float -> Shape'
baseRect' width height = Rectangle' (Point 0 0) (Point width height)


data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)



-- the 'a' here is the type parameter. Because a type parameter is involved
-- Maybe' is a type constructor. Depending on what we want the data type to hold
-- when its not Nothing, this type constructor can end up producing a type Maybe' Int
-- or Maybe' Point or Maybe' Person
-- No value can have a type of just Maybe'
-- this is just like supplying a generic argument in Java or C++
data Maybe' a = Nothing | Just a


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = (i * l) + (j * m) + (k * n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)
