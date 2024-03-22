{-
Homework 3 - March 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- Question 1 --
-- diffStr "Haskell" "the best" == "Hakll"
-- diffStr "Cannot predict" "the future" == "Cannopdic"
-- diffStr "Be the type of person" "you want to meet" == "Bhpfprs"
-- diffStr "Eotvos Lorand" "University" == "Eoo Load"
diffStr :: String -> String -> String
diffStr str1 str2 = [c | c <- str1, c `notElem` str2]

-- Question 2 --
-- splitList [(1, False), (2, True), (3, False)] == ([1, 2, 3], [False, True, False])
-- splitList [(4, True)] == ([4], [True])
-- splitList [] == ([], [])
splitList :: [(Int, Bool)] -> ([Int], [Bool])
splitList xs = ([num | (num, _) <- xs], [bool | (_, bool) <- xs])

-- Question 3 --
-- doubleEverySecond [1, 2, 3, 5, 6, 7] == [1, 4, 3, 10, 6, 14]
-- doubleEverySecond [5, 6, 7] == [5, 12, 7]
-- doubleEverySecond [5] == [5]
doubleEverySecond :: [Int] -> [Int]
doubleEverySecond (x:y:xs) = x : 2 * y : doubleEverySecond xs
doubleEverySecond xs = xs

-- Question 4 --
-- length calendar == 365
-- (31, 1) `elem` calendar == True
-- (32, 1) `elem` calendar == False
-- (28, 2) `elem` calendar == True
-- (29, 2) `elem` calendar == False
-- (30, 4) `elem` calendar == True
-- (31, 4) `elem` calendar == False
-- Helper function to determine the number of days in a given month
daysInAMonth :: Int -> Int
daysInAMonth month
    | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | month `elem` [4, 6, 9, 11] = 30
    | month == 2 = 28
    | otherwise = error "Invalid month"

calendar :: [(Int, Int)]
calendar = [(day, month) | month <- [1..12], day <- [1..(daysInAMonth month)]]

-- Question 5 --
-- 0 `mul` 5 == 0
-- 5 `mul` 0 == 0
-- 3 `mul` 4 == 12
-- 4 `mul` 3 == 12
-- 10 `mul` 10 == 100
mul :: Int -> Int -> Int
mul x 0 = 0
mul x 1 = x
mul x y = x + mul x (y - 1)

-- Question 6 --
-- doubleChars "Hello!" == "HHeelllloo!!"
-- doubleChars "abcd" == "aabbccdd"
-- doubleChars "x" == "xx"
-- doubleChars "" == ""
doubleChars :: String -> String
doubleChars str = concat [replicate 2 c | c <- str]

-- Question 7 --
-- elem' 'l' "Haskell" == True
-- elem' 'v' "Lujzi" == False
-- elem' 'x' "" == False
elem' :: Char -> String -> Bool
elem' c "" = False
elem' c (x:xs) = x == c || elem' c xs

-- Question 8 --
-- countChar 'l' "Haskell" == 2
-- countChar 'c' "abcabcabc" == 3
-- countChar 'x' "" == 0
countChar :: Char -> String -> Int
countChar _ "" = 0
countChar c (x:xs)
    | x == c = 1 + countChar c xs 
    | otherwise = countChar c xs