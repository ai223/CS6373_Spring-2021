doubleMe x = x + x

doubleUs x y = (doubleMe x) + (doubleMe y)

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
