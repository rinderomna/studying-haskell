{-
Homework 8 - May 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- 1
data Vector3 = V Int Int Int
    deriving (Show, Eq)

-- 2
-- componentSum (V 4 6 7) == 17
-- componentSum (V 0 0 0) == 0
-- componentSum (V (-1) 45 (-55)) == (-11)
componentSum :: Vector3 -> Int
componentSum (V x y z) = x + y + z

-- 3
-- crossProduct (V 5 6 7) (V 3 4 5) == V 2 (-4) 2
-- crossProduct (V 1 2 3) (V 3 2 1) == V (-4) 8 (-4)
-- crossProduct (V 1 1 8) (V 0 2 1) == V (-15) (-1) 2
-- crossProduct (V 7 4 3) (V 8 3 1) == V (-5) 17 (-11)
crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (V x1 y1 z1) (V x2 y2 z2) = V x y z
    where
        x = y1 * z2 - z1 * y2
        y = z1 * x2 - x1 * z2
        z = x1 * y2 - y1 * x2

-- 4
-- vectorListSum [] == V 0 0 0
-- vectorListSum [V 1 2 3] == V 1 2 3
-- vectorListSum [V 1 1 1, V 0 0 0, V 2 2 3] == V 3 3 4
-- vectorListSum [V 0 2 4, V (-10) 4 2, V 0 0 0] == V (-10) 6 6
-- vectorListSum [V 0 0 0, V 0 0 0, V 0 0 0, V 0 0 0] == V 0 0 0
-- vectorListSum [V i (i*i) (i*i*i) | i <- [0..10]] == V 55 385 3025

-- vectorListSum :: [Vector3] -> Vector3
-- vectorListSum = foldl (\(V x1 y1 z1) (V x2 y2 z2) -> V (x1 + x2) (y1 + y2) (z1 + z2)) (V 0 0 0)

instance Num Vector3 where
    (V x1 y1 z1) + (V x2 y2 z2) = V (x1 + x2) (y1 + y2) (z1 + z2)
    fromInteger _ = V 0 0 0
    negate (V x y z) = V (-x) (-y) (-z)
    (*) = crossProduct
    abs (V x y z) = V (abs x) (abs y) (abs z)
    signum (V x y z) = V (signum x) (signum y) (signum z)

vectorListSum :: [Vector3] -> Vector3
vectorListSum = sum

-- 5
data Storage = HDD String Int Int | SSD String Int
    deriving (Show, Eq)

-- 6
-- capacity (HDD "Verbatim" 7200 1000) == 1000
-- capacity (SSD "Samsung" 500) == 500
-- capacity (SSD "Samsung" 750) == 750
capacity :: Storage -> Int
capacity (HDD _ _ cap) = cap
capacity (SSD _ cap) = cap

-- 7
-- isHDD (HDD "Seagate" 5600 250) == True
-- isHDD (HDD "Verbatim" 7200 1000) == True
-- isHDD (SSD "Samsung" 500) == False
-- isHDD (SSD "Samsung" 750) == False
isHDD :: Storage -> Bool
isHDD (HDD {}) = True
isHDD _ = False

-- 8
-- hugeHDDs [] == []
-- hugeHDDs [HDD "Seagate" 5600 250, HDD "Verbatim" 7200 1000, SSD "Samsung" 500, SSD "Samsung" 750] == [HDD "Verbatim" 7200 1000]
-- hugeHDDs [HDD "Seagate" 5600 250, HDD "Verbatim" 7200 100, SSD "Samsung" 250, SSD "Samsung" 100] == []
-- hugeHDDs [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500, SSD "Samsung" 250, SSD "Samsung" 500] == [HDD "Seagate" 5600 1000,HDD "Verbatim" 7200 1500]
hugeHDDs :: [Storage] -> [Storage]
hugeHDDs xs  = filter (\s -> isHDD s && capacity s > maxSSDCapacity) xs
    where
        maxSSDCapacity = maximum $ map capacity $ filter (not . isHDD) xs