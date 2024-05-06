import Language.Haskell.TH (safe)
-- 1
fact :: Integer -> Integer
fact n 
    | n < 0     = error "Negative numbers not supported"
    | n == 0    = 1
    | n == 1    = 1
    | otherwise = n * fact (n - 1)

fact' :: Integer -> Integer
fact' n | n < 0 = error "Negative numbers not supported"
fact' 0 = 1
fact' 1 = 1
fact' n = n * fact (n - 1)

-- 2
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 3 -- more efficient:
fibAux :: Integer -> (Integer, Integer) -- (prev, curr)
fibAux 0 = (0, 0)
fibAux 1 = (0, 1)
fibAux n = (y, x + y)
    where
        (x, y) = fibAux (n - 1) -- (x = prevPrev, y = prev)

fib' :: Integer -> Integer
fib' n = snd (fibAux n)

-- 4
pow :: Integer -> Integer -> Integer
pow a 0 = 1
pow a b = a * pow a (b - 1)

-- 5
range :: Int -> Int -> [Int]
range a b
    | a < b = []
    | a == b = [a]
    | otherwise = a : range (a + 1) b  

-- 6
range' :: Int -> Int -> [Int]
range' a b 
    | a == b = [a]
    | a < b = a : range' (a + 1) b
    | a > b = a : range'(a - 1) b

-- 7
length' :: [Int] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


-- 8
minimum' :: [Int] -> Int
minimum' [a] = a
minimum' (x:xs) 
    | x <= m = x
    | otherwise = m
    where
        m = minimum' xs

-- 9
everySecond :: String -> String
everySecond (x:y:xs) = y : everySecond xs
everySecond _ = ""

-- 10
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || elem' e xs

-- 11
value :: Int -> [(Int, String)] -> String
value _ [] = error "Key not found"
value x ((y, s):ys) 
    | x == y = s
    | otherwise = value x ys

-- 12
value' :: Int -> String -> [(Int, String)] -> String
value' _ d [] = d
value' x _ ((y, s):ys)
    | x == y = s
    | otherwise = value x ys