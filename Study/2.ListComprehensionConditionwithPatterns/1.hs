{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use list literal pattern" #-}

fact :: Integer -> Integer
fact 0 = 1
fact n
    | n > 0 = n * fact (n - 1)
    | otherwise = error ("fact: negative number " ++ show n)

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = x == e || elem' e xs
-- | e == x = True
-- | otherwise = elem' e xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

minimum' :: Ord a => [a] -> a
minimum' (x:[]) = x
minimum' (x:xs) = x `min` minimum' xs

{-
minimum' (x:[]) = x
minimum' (x:xs)
    | x < m = x
    | otherwise = m
    where
        m = minimum' xs
-}

----------------------------------------

-- List Comprehension Condition with Patterns

x :: [Char]
x = [e | (e, i) <- [('a', 1), ('b', 2), ('c', 1)],{->-}i == 1{-<-}]

y :: [Char]
y = [e | (e, {->-}1{-<-}) <- [('a', 1), ('b', 2), ('c', 1)]]

ans :: Bool
ans = x == y -- True

-----------------------------------------

-- every5 "Hello World!" == "H d"
every5 :: [a] -> [a]
every5 ls = [e | (i, e) <- zip [0..] ls, i `mod` 5 == 0]

every5' :: [a] -> [a]
every5' ls = [e | (1, e) <- zip (cycle [1..5]) ls]

everyN :: Int -> [a] -> [a]
everyN i ls = [e | (1, e) <- zip (cycle [1..i]) ls]

everyNth :: Int -> [a] -> [a]
everyNth i ls = [e | (1, e) <- zip (cycle [i, (i-1)..1]) ls]

-----------------------------------------

(+:+) :: [a] -> [a] -> [a]
(+:+) [] ks = ks
(+:+) (x:xs) ks = x : (+:+) xs ks

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverseT :: [a] -> [a]
reverseT ls = revT ls []
    where
        revT [] acc = acc
        revT (x:xs) acc = revT xs (x:acc)

take' :: Int -> [a] -> [a]
take' 0 ls = []
take' i [] = []
take' i (x:xs)
    | i > 0 = x : take' (i-1) xs
    | otherwise = []

-- HOMEWORK: drop [DONE]
drop' :: Int -> [a] -> [a]
drop' 0 ls = ls
drop' i [] = []
drop' i (x:xs)
    | i > 0 = drop' (i-1) xs
    | otherwise = x:xs

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _ = []

isShorterOrEqThan :: [a] -> [a] -> Bool
isShorterOrEqThan [] _ = True
isShorterOrEqThan _ [] = False
isShorterOrEqThan (_:xs) (_:ys) = isShorterOrEqThan xs ys

splitOn :: Ord a => a -> [a] -> ([a], [a])
splitOn e ls = splitH ls [] [] -- no need for the element, you can use in where
    where
        splitH [] le g = (reverse le, reverse g)
        splitH (x:xs) le g
            | x <= e    = splitH xs (x:le) g
            | otherwise = splitH xs le    (x:g)

splitOn' :: Ord a => a -> [a] -> ([a], [a])
splitOn' e [] = ([], [])
splitOn' e (x:xs)
    | x <= e    = (x:l1, l2)
    | otherwise = (l1, x:l2)
    where
        (l1, l2) = splitOn' e xs

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([], [])
unzip' ((x,y):xs) = (x:l1, y:l2)
    where
        (l1, l2) = unzip' xs

-------------------------------------------------
-- TODO: Compare efficience

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort le ++ [x] ++ qsort g
    where
        le = [e | e <- xs, e <= x]
        g  = [e | e <- xs, e > x]

qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' le ++ [x] ++ qsort' g
    where
        (le, g) = splitOn' x xs

qsort'' :: Ord a => [a] -> [a]
qsort'' []     = []
qsort'' (x:xs) = qsort'' le ++ [x] ++ qsort'' g
    where
        (le, g) = splitOn x xs