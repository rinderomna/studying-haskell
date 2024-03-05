import Data.List

-- 1
avg :: [Int] -> Double
avg [] = 0
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- 2
exchange :: Int -> Int
-- exchange euros = floor (362.82 * fromIntegral euros)
exchange euros = (euros * 36382) `div` 100

-- 3
divisors :: Int -> [Int]
divisors 0 = [1..] -- Warning!
divisors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

isPrime' :: Int -> Bool
isPrime' n
    | n <= 1 = False
    | otherwise = null divs
        where
            divs = [x | x <- [2..floor (sqrt (fromIntegral n))], n `rem` x == 0]

-- 4
primes :: [Int]
primes = [x | x <- [2..], isPrime x]

-- 5
dominoes :: [(Int, Int)]
dominoes = [(x, y) | x <- [0..6], y <- [0..6], x <= y]

-- 6 [todo]

-- 7 
alphabet :: [(Int, Char)]
--alphabet = zip [0..25] ['a'..'z']
alphabet = zip [0..] ['a'..'z'] -- Warning!

-- 8
everyThird :: String
everyThird = [c | (i, c) <- zip [1..] cs, i `mod` 3 == 0]
    where
        cs = ['a'..'z']

everyThird' :: String
everyThird' = [c | (b, c) <- zip (cycle [False, False, True]) cs, b]
    where
        cs = ['a'..'z']

-- 9
square :: Int -> Bool
square n = n `elem` take n squares
    where
        squares = [x ^ 2 | x <- [1..]]


square' :: Int -> Bool
square' n = floor(sqrt (fromIntegral n)) == floor(sqrt (fromIntegral n))
-- Maybe inacurate?

-- 10 --!
courses :: [(String, [(String, String, String)])]
courses = [("Calculus", [("Simon", "Jones", "BDE91E"), ("Barack", "Obama", "DDA3KX")]), 
    ("Imperative Programming", [("Simon", "Marlow", "ALX1K0"), ("John", "Hughes", "BDE91E")]), 
    ("Functional Languages", [("Philip", "Wadler", "ABCDE6"), ("Simon", "Thompson", "CDE560")])]

students :: [String]
students =
    [
        code |
        (courseName, students) <- courses,
        courseName == "Functional Languages",
        (_, _, code) <- students
    ]
-- Warning! Nested for loop!

studentsOf :: String -> [String]
studentsOf course =
    [
        code |
        (courseName, students) <- courses,
        courseName == course,
        (_, _, code) <- students
    ]


-- 11 -- [TODO]

-- 12
compress :: String -> [(Int, Char)]
compress s = [(length cs, head cs) | cs <- group s]

-- 13
decompress :: [(Int, Char)] -> String
decompress xs = concat [replicate n c | (n, c) <- xs]

decompress' :: [(Int, Char)] -> String
decompress' xs = 
    [
        c | 
        (n, c) <- xs, -- "outer for-loop"
        i <- [1..n] -- "inner for-loop"
    ]