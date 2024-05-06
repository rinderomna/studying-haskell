{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

-- 1
take' :: Int -> [a] -> [a]
take' _ [] = []
take' a (x:xs)
    | a <= 0 = []
    | otherwise = x : take' (a - 1) xs

-- 2
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' a (x:xs)
    | a <= 0 = x:xs
    | otherwise = drop' (a - 1) xs 

-- 3
langAndRegion :: String -> (String, String)
langAndRegion (x:xs) 
    | x == '-' = ("", xs)
    | otherwise = (x : ys, zs)
    where 
        (ys,zs) = langAndRegion xs 

langAndRegion' :: String -> (String, String)
langAndRegion' s = (take' 2 s, drop' 3 s)

-- 4
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- 5
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((y, z): xs) = (y: ys, z : zs)
    where 
        (ys, zs) = unzip' xs 

-- 6
empty :: String -> [Int]
empty s = [i | (i, line) <- zip' [1..] (lines s), null line]

-- 7
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop n xs)

splitAt'' :: Int -> [a] -> ([a], [a])
splitAt'' _ [] = ([], [])
splitAt'' n (x:xs) 
    | n <= 0 = ([], x:xs)
    | otherwise = (x:ys, zs)
    where
        (ys, zs) = splitAt'' (n - 1) xs

-- 8
nubAux :: Eq a => [a] -> [a] -> [a]
nubAux [] _ = []
nubAux (x:xs) used 
    | x `elem` used = nubAux xs used
    | otherwise = x : nubAux xs (x:used) 

nub' :: Eq a => [a] -> [a]
nub' xs = nubAux xs []

nub'' :: Eq a => [a] -> [a]
nub'' [] = []
nub'' (x:xs) = x : nub'' [y | y <- xs, y /= x]

-- 9
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss -- !

-- About Infinite Lists: --
-- Can be built by recursion!
zeroes :: [Int]
zeroes = 0 : zeroes

-- > take 5 zeroes
--  [0, 0, 0, 0, 0]

repeat' :: a -> [a]
repeat' e  = e : repeat' e
-- repeat' x = [x] ++ repeat' x
-- repeat' x = repeat' x ++ [x] -- Infinite Loop!

-- > data Whatever = A | B deriving Show
-- > take 5 (repeat' A)
--   [A,A,A,A,A]

-- So another cool definition of the Fibonacci function:
fib :: [Integer]
fib = 1 : 1 : [x + y | (x, y) <- zip fib (tail fib)]

-- > take 20 fib
--   [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
