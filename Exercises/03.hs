{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use guards" #-}

-- 1 -- [TODO]

-- 2 -- [TODO]

foo :: Int -> (Int -> Int)
foo x y = x + y

-- 3
xor :: Bool -> Bool -> Bool
xor False y = y
xor True y = not y

-- 4
add2 :: Int -> Int -> (Int, Int)
add2 0 0 = (0, 0)
add2 0 1 = (1, 0)
add2 1 0 = (1, 0)
add2 1 1 = (0, 1)

add2' :: Int -> Int -> (Int, Int)
add2' 1 1 = (0, 1)
add2' x y = (x + y, 0)

-- 5
paren :: Char -> Char -> Bool
paren '{' '}' = True
paren '(' ')' = True
paren '[' ']' = True
paren _ _ = False

-- 6
calc :: (Integer, Char, Integer) -> Integer
calc (x, '+', y) = x + y
calc (x, '-', y) = x - y
calc (x, '*', y) = x + y
calc (x, '/', y) = x `div` y

-- 7 
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False

-- Obs:
empty :: ()
empty = ()

one :: (Int) -- = Int
one = 1

nested :: (Int, (Char, Bool))
nested = (1, ('c', True))

---- absolute value with pattern matching:
f :: Bool -> Int -> Int
f True x = x
f False x = -x

abs' :: Int -> Int
abs' x = f (x >= 0) x

--- abs' (-4) -> f (-4 >= 0) (-4) -> f False (-4) ->(match)-> -(-4) -> 4

---- absolute value with if-then-else:
abs'' :: Int -> Int
abs'' x = if x >= 0 then x else -x

-- If then else nested:
g :: Int -> Int
g x = if x <= 0 then x + 2 else if x <= 3 then x * 2 else x

-- With guards:
g' :: Int -> Int
g' x
    | x <= 0 = x + 2
    | x <= 3 = x * 2
    | otherwise = x

