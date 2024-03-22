{-
Homework 4 - March 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- 1
-- doubleAll [1, 2, 3] == [2, 4, 6]
-- doubleAll [6, -4, 0, 5] == [12, -8, 0, 10]
-- doubleAll [] == []
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = 2 * x : doubleAll xs

-- 2
-- insertAt 3    'l' "word" == "world"
-- insertAt 4    'o' "hell" == "hello"
-- insertAt 10   'o' "hell" == "hello"
-- insertAt 0    'h' "ello" == "hello"
-- insertAt (-3) 'h' "ello" == "hello"
insertAt :: Int -> Char -> String -> String
insertAt _ c [] = [c]
insertAt i c (x:xs)
    | i <= 0 = c : x : xs
    | otherwise = x : insertAt (i - 1) c xs

-- 3
-- isSorted [2, 3, 5, 8] == True
-- isSorted [2, 5, 3, 8] == False
-- isSorted [8, 5, 3, 2] == False
-- isSorted [] == True
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- 4
-- chunks 4 "helloworld" == ["hell", "owor", "ld"]
-- chunks 2 "helloworld" == ["he", "ll", "ow", "or", "ld"]
-- chunks 5 "abc" == ["abc"]
-- chunks 5 "" == []
chunks :: Int -> String -> [String]
chunks _ [] = []
chunks n s = take n s : chunks n (drop n s)

-- 5
-- isLonger [1, 2, 3] [4, 5]    == True
-- isLonger "Java"    "Haskell" == False
-- isLonger [1, 2]    [3, 4]    == False
-- isLonger [1..]     [4, 5]    == True
-- isLonger [1, 2, 3] [1..]     == False
isLonger :: [a] -> [a] -> Bool
isLonger [] [] = False
isLonger [] _ = False
isLonger _ [] = True
isLonger (x:xs) (y:ys) = isLonger xs ys

-- 6
-- append [1, 2, 3] [4, 5] == [1, 2, 3, 4, 5]
-- append "hello" "world" == "helloworld"
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- 7
-- reverse' [1, 2, 3, 4] == [4, 3, 2, 1]
-- reverse' "hello" == "olleh"
-- reverse' "Racecar" == "racecaR"
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = append (reverse' xs) [x]

-- 8 
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = append smaller (x : larger)
  where
    smaller = quicksort [y | y <- xs, y <= x]
    larger  = quicksort [y | y <- xs, y > x]