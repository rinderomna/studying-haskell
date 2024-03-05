{-
Homework 2 - March 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- Question 1 --
-- third [2, 3, 4] == 4
-- third [3, 8, 5, 9, 1] == 5
-- third [1 ..] == 3
-- third [1, 2] == 0
third :: [Int] -> Int
third (_ : _ : x : _) = x
third _ = 0

-- Question 2 --
-- addFirstTwo [1, 2, 5, 7, 8] == [3, 5, 7, 8]
-- addFirstTwo [12, 13, 0, 1, 2] == [25, 0, 1, 2]
-- addFirstTwo [4, 5] == [9]
-- addFirstTwo [1] == [1]
addFirstTwo :: [Int] -> [Int]
addFirstTwo (a : b : t) = a + b : t
addFirstTwo x = x

-- Question 3 --
-- factorial 3  == 6     -- 1 * 2 * 3
-- factorial 4  == 24    -- 1 * 2 * 3 * 4
-- factorial 10 == 3628800
-- factorial 1  == 1
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Question 4 --
-- sumOfSquares 2 == 5     -- 1 + 4
-- sumOfSquares 3 == 14    -- 1 + 4 + 9
-- sumOfSquares 4 == 30    -- 1 + 4 + 9 + 16
-- sumOfSquares 0 == 0
sumOfSquares :: Int -> Int
sumOfSquares n = sum [x ^ 2 | x <- [1..n]]

-- Question 5 --
-- sums [10, 20, 30] [1, 2, 3] == [11, 12, 13, 21, 22, 23, 31, 32, 33]
-- sums [2, 3] [6, 9] == [8, 11, 9, 12]
-- sums [1] [2, 3] == [3, 4]
-- sums [1, 2, 3] [] == []
sums :: [Int] -> [Int] -> [Int]
sums xs ys = [x + y | x <- xs, y <- ys]

-- Question 6 --
-- evens [0 .. 9] == [0, 2, 4, 6, 8]
-- evens [30, 11, 14] == [30, 14]
-- evens [5, 3, 1] == []
-- evens [2, 4, 8] == [2, 4, 8]
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, x `mod` 2 == 0]

-- Question 7 --
-- countEvens [0 .. 9] == 5
-- countEvens [30, 11, 14] == 2
-- countEvens [5, 3, 1] == 0
-- countEvens [2, 4, 8] == 3
countEvens :: [Int] -> Int
countEvens xs = length (evens xs)

-- Question 8 --
-- allEven [2, 4, 8] == True
-- allEven [2, 4, 7, 8] == False
-- allEven [] == True
allEven :: [Int] -> Bool
allEven xs = length xs == countEvens xs

-- Bonus 1 --
-- allEqualTo 3 [3, 3, 3, 3] == True
-- allEqualTo 1 [1, 1, 2, 1] == False
-- allEqualTo 5 [] == True
allEqualTo :: Int -> [Int] -> Bool
allEqualTo n xs = null [x | x <-xs, x /= n]

-- Bonus 2 --
-- allEqual [3, 3, 3, 3] == True
-- allEqual [1, 1, 2, 1] == False
-- allEqual [] == True
allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x : xs) = allEqualTo x xs

-- Bonus 3 --
pi' :: Double
pi' = 4 * sum (take 1000 [x ** (-1) | x <- l])
    where l = [(-1) ** n * (2 * n + 1) | n <- [0..]]
