{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import Distribution.Simple.Utils (xargs)
import Control.Arrow (ArrowLoop(loop))
{-# HLINT ignore "Use foldr" #-}

-- Turners Sieve: (https://wiki.haskell.org/Prime_numbers#Turner.27s_sieve_-_Trial_division)
primesAux :: [Int] ->[Int]
primesAux (x:xs) = x : primesAux [y | y <-xs, y `mod` x /= 0]

primes :: [Int]
primes = primesAux [2..]

-- Sum
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- > sum' [1..10000000]

-- Sum with Tail Recursion (no stackoverflow, but slow in ghci)
sum'' :: [Integer] -> Integer
sum'' xs = loop 0 xs
    where   
        loop :: Integer -> [Integer] -> Integer
        loop acc [] = acc
        loop acc (x:xs) = loop (acc + x) xs

-- ^ lazy: H. won't evaluate (acc + x)
-- acc should use strict evaluation

-- For compilation
main :: IO ()
main = print (sum'' [1..10000000])

-- Maximum without tail recursion:
maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs)
    | x >= y = x
    | otherwise = y
    where  
        y = maximum' xs

-- Maximum with tail recursion:
maximum'' :: Ord a => [a] -> a
maximum'' (x:xs) = loop x xs
    where
        loop :: Ord a => a -> [a] -> a
        loop maxSoFar [] = maxSoFar
        loop maxSoFar (x:xs)
            | maxSoFar >= x = loop maxSoFar xs
            | otherwise     = loop x xs

-- See the pattern? We will use high order function foldl!

-- Reverse of a list:
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- ^ One problem with this definition: very slow O(n^2)
-- reverse' [1, 2, 3, 4] = ((([] ++ [4]) ++ [3]) ++ [2]) ++ [1]
-- The problem is the that parentheses are left-put

reverse'' :: [a] -> [a]
reverse'' xs = loop xs []
    where
        loop [] acc = acc
        loop (x:xs) acc = loop xs (x:acc)

{-
reverse'' [1, 2, 3]      =
loop [1, 2, 3] []        =
loop [2, 3]    [1]       = 
loop [3]       [2, 1]    =
loop []        [3, 2, 1] =
[3, 2, 1]
-}

-- ^ Tail recursive version: efficient O(n) [linear]

-- Fibbonaci (tail recursive version)
fib :: Integer -> Integer
fib n = loop 0 1 n
    where 
        loop prev curr 1 = curr
        loop prev curr n = loop 
            curr{-new_previous-} 
            (prev + curr){-new_current-} 
            (n-1){-one less iteration left-}

-- HIGHER ORDER FUNCTIONS:

-- Function that applies a function twice
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- > twice (+1) 0
-- 2

-- Search: **Operator Section** => (+1) is an operator section

-- Another way: **Lambda expression**

-- > twice (\x -> x + 1) 0
-- 2

-- Or **functions per se**

add1 :: Int -> Int
add1 x = x + 1

-- > twice add1 0
-- 2

-- List of functions:
listOfFunctions :: [Int -> Int]
listOfFunctions = [add1, (* 2), \x -> 2 * x + 1]

list2 :: [a -> a]
list2 = [id] -- only that fits

sub1 :: Int -> Int
-- sub1 = (- 1) [WRONG]
-- sub1 = (+ (-1)) [RIGHT]
sub1 = subtract 1 

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = \b a -> f a b
flip' f b a = f a b

subtract' :: Integer -> Integer -> Integer
subtract' = flip (-)

{-
twice (subtract 1) 2
twice (flip (-) 1) 2
twice ((\b a -> (-) a b) 1) 2
twice ((\b a -> a - b) 1) 2
twice (\a -> a - 1) 2
0
-}

splitFun :: (a -> (b, c)) -> (a -> b, a -> c)
splitFun f = (\a -> fst (f a), \a -> snd (f a))

-- > fst (splitFun (\x -> (x + 1, x + 2))) 0 
-- 1
-- > snd (splitFun (\x -> (x + 1, x + 2))) 0 
-- 2

compose :: (b -> c) -> (a -> b) -> (a -> c) -- (.)
compose f g a = f (g a)

add3 :: Int -> Int
add3 = compose (+1) (+2)

add3' :: Int -> Int
add3' = (+1) . (+2)

splitFun' :: (a -> (b, c)) -> (a -> b, a -> c)
splitFun' f = (fst . f, snd . f)

