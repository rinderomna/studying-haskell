{-
Homework 1 - February 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- Question 1 --
-- cmpRem5Rem7 4  == False
-- cmpRem5Rem7 6  == False
-- cmpRem5Rem7 7  == True
-- cmpRem5Rem7 9  == True
-- cmpRem5Rem7 10 == False
cmpRem5Rem7 :: Int -> Bool
cmpRem5Rem7 x = x `mod` 5 > x `mod` 7

-- Question 2 --
-- isSmallPrime 1  == False
-- isSmallPrime 2  == True
-- isSmallPrime 6  == False
-- isSmallPrime 7  == True
-- isSmallPrime 11 == False
isSmallPrime :: Int -> Bool
isSmallPrime 2 = True
isSmallPrime 3 = True
isSmallPrime 5 = True
isSmallPrime 7 = True
isSmallPrime _ = False

-- Question 3 --
equivalent :: Bool -> Bool -> Bool
equivalent True  True  = True
equivalent True  False = False
equivalent False True  = False
equivalent False False = True

-- Question 4 --
implies :: Bool -> Bool -> Bool
implies True  False = False
implies _ _  = True

-- Question 5 --
-- invertO (1, 2)  == (-1, -2)
-- invertO (3, -7) == (-3, 7)
-- invertO (0, 0)  == (0, 0)
invertO :: (Int, Int) -> (Int, Int)
invertO (x, y) = (-x, -y)

-- Question 6 --
-- isOnNegId (2, -2) == True
-- isOnNegId (-3, 3) == True
-- isOnNegId (0, 0)  == True
-- isOnNegId (1, 1)  == False
-- isOnNegId (4, -5) == False
isOnNegId :: (Int, Int) -> Bool
isOnNegId (x, y) = y == (-x)

-- Question 7 --
-- (1, 2)   `divide` (2, 1) == (1, 4)
-- (2, 3)   `divide` (1, 2) == (4, 3)
-- (-7, 11) `divide` (4, 3) == (-21, 44)
divide :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
divide (n1, d1) (n2, d2) = (n1 * d2 , d1 * n2)

-- Question 8 --
-- Helper function to add fractions:
add :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
add (x1, y1) (x2, y2) = (x1 * y2 + x2 * y1, y1 * y2)
-- (2, 3)   `sub` (1, 2) == (1, 6)
-- (5, 1)   `sub` (3, 2) == (7, 2)
-- (-7, 11) `sub` (4, 3) == (-65, 33)
sub :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sub f1 (x2, y2) = f1 `add` (-x2, y2)

-- Question 9 --
-- swap ('a', 'b') == ('b', 'a')
-- swap ('O', 'K') == ('K', 'O')
-- swap ('?', '!') == ('!', '?')
swap :: (Char, Char) -> (Char, Char)
swap (x, y) = (y, x)