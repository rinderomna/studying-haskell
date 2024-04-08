{-
Homework 5 - April 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- Auxiliary Reverse Function:
reverse' :: [a] -> [a]
reverse' xs = loop xs []
    where
        loop [] acc = acc
        loop (x:xs) acc = loop xs (x:acc)

-- 1
-- take 5 (arithSeq 1 2)    == [1, 3, 5, 7, 9]
-- take 5 (arithSeq 7 10)   == [7, 17, 27, 37, 47]
-- take 5 (arithSeq 9 (-1)) == [9, 8, 7, 6, 5]
-- take 5 (arithSeq 3 0)    == [3, 3, 3, 3, 3]
arithSeq :: Int -> Int -> [Int]
arithSeq a n = a : arithSeq (a + n) n

-- 2
-- validChain "soup peas sugar rice" == True
-- validChain "car red dig get tin"  == True
-- validChain "apple elephant zoo"   == False
validChain :: String -> Bool
validChain s = check (words s)
    where
        check :: [String] -> Bool
        check [] = True
        check [x] = True
        check (x:y:xs) = last x == head y && check (y:xs)

-- 3
-- removeExtraSpace "quick     brown   fox" == "quick brown fox"
-- removeExtraSpace "a        b" == "a b"
-- removeExtraSpace "   x   " == " x "
removeExtraSpace :: String -> String
removeExtraSpace = loop False []
    where
        loop :: Bool -> String -> String -> String
        loop _ acc [] = reverse' acc
        loop False acc (x:xs) = loop (x == ' ') (x:acc) xs
        loop True acc (x:xs)
            | x == ' ' = loop True acc xs
            | otherwise = loop False (x:acc) xs

-- 4
-- elemIndex 'o' "hello world" == 4
-- elemIndex 3 [1, 4, 3, 7, 3] == 2
-- elemIndex 1 [1, 2, 3, 1, 2] == 0
elemIndex :: Eq a => a -> [a] -> Int
elemIndex e xs = loop 0 e xs
    where
        loop :: Eq a => Int -> a -> [a] -> Int
        loop _ _ [] = -1
        loop acc e (x:xs)
            | x == e = acc
            | otherwise = loop (acc + 1) e xs

-- 5
-- deleteLast 'o' "hello world" == "hello wrld"
-- deleteLast 3 [1, 4, 3, 7, 3] == [1, 4, 3, 7]
-- deleteLast 1 [1, 2, 3, 1, 2] == [1, 2, 3, 2]
-- deleteLast 0 [1, 2, 3] == [1, 2, 3]
lastIndex :: Eq a => a -> [a] -> Int
lastIndex e xs = loop (-1) 0 e xs
    where
        loop :: Eq a => Int -> Int -> a -> [a] -> Int
        loop acc _ _ [] = acc
        loop acc idx e (x:xs)
            | x == e = loop idx (idx + 1) e xs
            | otherwise = loop acc (idx + 1) e xs

removeByIndex :: Eq a => Int -> [a] -> [a]
removeByIndex _ [] = []
removeByIndex (-1) xs = xs
removeByIndex 0 (x:xs) = xs
removeByIndex idx (x:xs) = x : removeByIndex (idx - 1) xs

deleteLast :: Eq a => a -> [a] -> [a]
deleteLast e xs = removeByIndex (lastIndex e xs) xs

-- 6
-- applyWhen True  (+ 1) 0 == 1
-- applyWhen False (+ 1) 0 == 0
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen _ _ = id

-- 7
-- curry' fst 1 2 == 1
-- curry' (\(x, y) -> x + y) 1 2 == 3
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

-- 8
-- uncurry' (+) (1, 2) == 3
-- uncurry' (\x y -> x + y) (1, 2) == 3
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

-- 9
-- mergeFun ((+ 1), (+ 2)) 0 == (1, 2)
mergeFun :: (a -> b, a -> c) -> (a -> (b, c))
mergeFun (f, g) x = (f x, g x)

-- 10
-- group "Mississippi" == ["M","i","ss","i","ss","i","pp","i"]
-- group [1, 1, 1, 2, 2, 3, 1, 5, 5] == [[1,1,1],[2,2],[3],[1],[5,5]]
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = loop x [[]] (x:xs)
    where
        loop :: Eq a => a -> [[a]] -> [a] -> [[a]]
        loop _ acc [] = reverse' acc
        loop curr (acc:accs) (x:xs)
            | x == curr = loop curr ((x:acc):accs) xs
            | otherwise = loop x ([x]:acc:accs) xs