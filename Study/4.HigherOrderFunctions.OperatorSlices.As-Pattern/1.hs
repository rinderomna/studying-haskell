{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use span" #-}

-- Mon 22 Apr 12:15

-- Remember currying
-- * f. 1 argument
-- * Can be partially applied

-- Simple functions have a template:
-- * f [] = {base_case}
-- * f (x:xs) = x {an operator} f xs

-- Redefine foldr as foldR
foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f e []     = e
foldR f e (x:xs) = x `f` foldR f e xs

-- (# `f` (# `f``(# `f ...(# `f` e)))) -- Right Associative ^

-- Example why (a -> b -> b) not (a -> a -> a):
(+::+) :: [a] -> [a] -> [a]
(+::+) ls ks = foldR (:) ks ls

-- Left associative foldL' later on ...

-- Redefine length function:
length' :: [a] -> Int
length' = foldR (\_ acc -> 1 + acc) 0

{-
length' [1, 2]
(foldr (\_ acc -> 1 + acc) 0) [1, 2]
-}

-------------------------------------------------------------------
-- SLIDES 07
-------------------------------------------------------------------
-- Higher order functions
-- We cannot show or compare functions
-- The 'map' function :: (a -> b) -> [a] -> [b]

{-
ghci> :t map (+1)
    map (+1) :: Num b => [b] -> [b]
ghci> :t map (+1)
    map (+1) :: Num b => [b] -> [b]
ghci> :t map even
    map even :: Integral a => [a] -> [Bool]
-}

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- * Operator Slices
{-
ghci> :t flip take [1..]
    flip take [1..] :: (Num a, Enum a) => Int -> [a]
ghci> :t (`take` [1..])                              --> [[Operator Slice!]]
    (`take` [1..]) :: (Num a, Enum a) => Int -> [a]
ghci> :t (`take` [1,42,73])                          --> [[Operator Slice!]]
    (`take` [1,42,73]) :: Num a => Int -> [a]
ghci> (^ 2) 10                                       --> [[Operator Slice!]]
    100
-}

-- (`take` [1..])           === 
-- (\i -> i `take` [1..10]) === 
-- (\i -> take i [1..10])   ===
-- (flip take [1..10])

allEqual :: Eq a => [a] -> Bool
allEqual xs = not $ any (head xs /=) xs

-- Alternative map definition with list comprehension:
map'' :: (a -> b) -> [a] -> [b] -- (different)/= a -> b -> [a] -> [b] == a -> (b -> ([a] -> [b]))
map'' f list = [f x | x <- list]

-- Filter function:
filter' :: (a -> Bool) -> [a] -> [a]
filter' pred [] = []
filter' pred (x:xs)
    | pred x    = x : filter' pred xs
    | otherwise = filter' pred xs

-- Alt:
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred list = [x | x <- list, pred x]

-- FilterMap:
filterMap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap f pred ls = [f x | x <- ls, pred x]

------------------------------------------------------------------
-- * span function:
-- span' :: (a -> Bool) -> [a] -> ([a], [a])
-- span' p xs = (takeWhile p xs, dropWhile p xs) -- not best implementation

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' pred [] = ([], [])
span' pred ls@(x:xs)
    | pred x = (x:l1, l2)
    | otherwise = ([], ls)
    where
        (l1, l2) = span' pred xs

--  As-pattern (@) can go further ls@(x:t@y:xs)

testAt :: [a] -> [[a]]
testAt ls@(x:ts@(y:xs)) = [ls, ts, xs] -- () needed!

{-
ghci> testAt [1..4]
    [[1,2,3,4],[2,3,4],[3,4]]
-}

-- Tail recursive version of span:
span'' :: (a -> Bool) -> [a] -> ([a], [a])
span'' pred ls = h ls []
    where
        h []     acc = (reverse acc, [])     -- need to reverse
        h ls@(x:xs) acc
            | pred x = h xs (x:acc)
            | otherwise = (reverse acc, ls)  -- need to reverse 

-- Iterate function:
{-
ghci> take 10 $ iterate (*2) 1
    [1,2,4,8,16,32,64,128,256,512]
-}

iterate' :: (a -> a) -> a -> [a]
iterate' f e = e : iterate' f  (f e)