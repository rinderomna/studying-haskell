{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
-- [TODO]: 1-6

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \a b -> f (g a b)
-- 7
--dotProduct :: Num a => [a] -> [a] -> a
--dotProduct xs ys = sum $ zipWith (*) xs ys

dotproduct :: [Integer] -> [Integer] -> Integer
dotproduct = sum .: zipWith (*)

-- 8
isPrime :: Int -> Bool
isPrime x
    | x <= 1 = False
    | otherwise = all (\y -> x `mod` y /= 0) [2..ceiling $ sqrt $ fromIntegral x]

isPrime' :: Int -> Bool
isPrime' x = length (filter (\y -> x `mod` y == 0) [1..x]) == 2

-- 9
primes :: [Int]
primes = filter isPrime [2..]

-- 10
iterate' :: (a -> a) -> a -> [a]
iterate' f e = e : iterate' f (f e)
-- 11
fibonacci :: [Int]
fibonacci = map fst $ iterate' (\(a, b) -> (b, a + b)) (0, 1)

----------------------------------------------------------
--- 2 more high order functions:
-- foldr and foldl

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- Generalizes recursion on a list

-- product = foldr (*) 1
-- sum = foldr (+) 0

-- foldr (+) 0 [1,2,3]
-- foldr (+) 0 (1:(2:(3:[])))
-- : turns into + and [] turns into 0
-- (1:(2:(3:[])))
-- (1+(2+(3+ 0)))

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f e [] = e
foldr_ f e (x:xs) = f x (foldr_ f e xs)

and :: [Bool] -> Bool
and = foldr_ (&&) True

map' :: (t -> a) -> [t] -> [a]
map' f = foldr_ (\x ys -> f x : ys) []

-- foldl is generalization of tail recursion

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ f = g
    where
        g a []    = a
        g a (x:l) = g (f a x) l

---------------------------------------------------
-- Data
data Color = Red | Green | Blue  -- Constructors
    deriving (Show)
-- Red   :: Color
-- Green :: Color
-- Blue  :: Color

isRed :: Color -> Bool
isRed Red = True      -- Pattern Match!
isRed _   = False

{-
ghci> isRed Red
    True
ghci> isRed Blue
    False
-}

swapColors :: Color -> Color
swapColors Blue  = Red
swapColors Red   = Green
swapColors Green = Blue
