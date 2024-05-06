{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use list literal pattern" #-}
------------------------------------------------
-- Imports --
------------------------------------------------
import Data.Char

-- lists in Haskel
x :: [Int]
x = []

y :: [Int]
y = [1]

z :: [Int]
z = [1, 2, 3, 3, 4]

-- They are linked lists
-- [1, 2] notation is syntantic sugar

z' :: [Int]
z' = 1 : (2 : (3 : (3 : (4 : []))))

-- : cons
-- [] nil

-- Right associative:
z'' :: [Int]
z'' = 1 : 2 : 3 : 3 : 4 : []

range :: [Int]
range = [5..10] -- inclusive

-- Laziness allow building infinite lists:
infrange :: [Int]
infrange = [1..]

{-
take 10 infrange
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-}

--------------------------------------------------

-- 1
headInt :: [Int] -> Int
headInt (x : _) = x

-- 2
tailInt :: [Int] -> [Int]
tailInt (_ : xs) = xs

-- 3
nullInt :: [Int] -> Bool
nullInt [] = True
nullInt _ = False

-- 4
isSingletonInt :: [Int] -> Bool
isSingletonInt [_] = True
isSingletonInt _ = False

isSingletonInt' :: [Int] -> Bool
isSingletonInt' (_ : []) = True
isSingletonInt' _ = False

-- 5
---- In Haskell, strings are just a list of characters
toUpperFirst :: String -> String
toUpperFirst [] = [] -- Warning!
toUpperFirst (x : xs) = toUpper x : xs

-- 6
---- 'elem' function to check if element is in list
isLetter :: Char -> Bool
isLetter c = elem c ['A'..'Z'] || elem c ['a'..'z']

-- 7
isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

-- 8
mountain :: Int -> [Int]
mountain n = [1 .. n-1] ++ [n, n-1 .. 1]

-- 9 
---- (list comprehension)
ex :: [Int]
ex = [1, 2, 3, 4]

ex2 :: [Int]
ex2 = [x * 2 | x <- ex]

ex3 :: [Int]
ex3 = [x * 2 | x <- ex, x /= 3]
----

divisors :: Int -> [Int]
divisors 0 = [1..] -- Warning!
divisors n = [x | x <- [1..n], n `mod` x == 0]

-----------------------------------
---- Obs: diff between mod and rem:
-- 5 mod (-3)
--  -1
-- 5 rem (-3)
--  2
-----------------------------------

-- 10
powersOfTwo :: [Int]
powersOfTwo = [2 ^ x | x <- [0..]]

-- 11 
l :: [Int]
l = [(-1) ^ n * (2 * n + 1)| n <- [0..]]

piDouble :: Double
piDouble = 4 * sum (take 1000 [fromIntegral x ** (-1) | x <- l])

-- 12
time :: [(Int, Int)]
time = [(hour, min) | hour <- [0..23], min <- [0..59]]