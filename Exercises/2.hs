-- 1
distance :: Integer -> Integer -> Integer
distance x y = abs (x - y)

-- 2
type Fraction = (Integer, Integer)
add :: Fraction -> Fraction -> Fraction
add (x1, y1) (x2, y2) = (x1 * y2 + x2 * y1, y1 * y2)

-- 3
mul :: Fraction -> Fraction -> Fraction
mul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

-- 4
modDiv :: Integer -> Integer -> (Integer, Integer)
modDiv x y = (x `mod` y, x `div` y) -- div for integer division

-- 5
quadratic :: Double -> Double -> Double -> (Double, Double)
{-
quadratic a b c = 
    ((-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a), 
     (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
-}
quadratic a b c = ((-b - d) / (2 * a), (-b + d) / (2 * a))
    where d = sqrt (b ^ 2 - 4 * a * c)

-- 6
matches :: (Int, Int) -> (Int, Int) -> Bool
matches (x1, y1) (x2, y2) = x1 == x2 || x1 == y2 || y1 == x2 || y1 == y2

-- 7
type Vector = (Int, Int)
len :: Vector -> Double
len (x, y) = sqrt (fromIntegral(x ^ 2 + y ^ 2)) 
{- You need to convert Int to Double with fromIntegral -}

-- 8
stretch :: Vector -> Int-> Vector
stretch (x, y) c = (c * x, c * y)

-- 9 
diff :: Vector -> Vector -> Vector
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

distance' :: Vector -> Vector -> Double
distance' x y = len (diff x y)