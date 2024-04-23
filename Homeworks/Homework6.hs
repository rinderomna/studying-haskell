{-
Homework 6 - April 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- 1
-- concatMap' (\c -> [c, c]) "Hello" == "HHeelllloo"
-- concatMap' (\x -> [x, x + 10]) [1, 2, 4] == [1, 11, 2, 12, 4, 14]
-- concatMap' (\n -> replicate n n) [2, 1, 3] == [2, 2, 1, 3, 3, 3]
-- concatMap' (\x -> [x]) [] == []
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f xs = loop f xs []
    where
        loop :: (a -> [b]) -> [a] -> [b] -> [b]
        loop _ [] acc = acc
        loop f (x:xs) acc = loop f xs (acc ++ f x)

-- 2
-- intersperse' ',' "abcde" == "a,b,c,d,e"
-- intersperse' 1 [3, 4, 5] == [3,1,4,1,5]
-- intersperse' 3 [] == []
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' e xs = init $ concatMap' (\x -> [x, e]) xs

-- 3
-- dropWhileEnd' (== ' ') "Hello world   " == "Hello world"
-- dropWhileEnd' (< 0) [1, -2, 3, 0, -1, -4] == [1, -2, 3, 0]
-- dropWhileEnd' (\_ -> True) [1, 2, 3] == []
-- dropWhileEnd' (\_ -> False) [] == []
dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' f xs = take (length xs - n) xs
    where
        countEndMatch :: (a -> Bool) -> [a] -> Int -> Int
        countEndMatch _ [] acc = acc
        countEndMatch p (x:xs) acc
            | p x = countEndMatch p xs (acc + 1)
            | otherwise = countEndMatch p xs 0

        n = countEndMatch f xs 0

-- 4
-- nestedMap (+ 1) [[1, 2], [], [3]] == [[2, 3], [], [4]]
-- nestedMap (== 'l') ["he", "llo"] == [[False, False], [True, True, False]]
-- nestedMap (+ 1) [] == []
-- nestedMap (+ 1) [[], [], []] == [[], [], []]
nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap _ [] = []    
nestedMap f (xs:xss) = map f xs : nestedMap f xss

-- 5
-- zipWith' (+) [1, 2, 3] [20, 30, 40] == [21, 32, 43]
-- zipWith' (+) [1..] [20, 30, 40, 50] == [21, 32, 43, 54]
-- zipWith' (\x y -> x) [1..] [5, 2, 6] == [1, 2, 3]
-- zipWith' (+) [] [1, 2] == []
-- zipWith' (+) [1, 2] [] == []
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 6
-- lookups "Bob" [("Alice", 4), ("Bob", 5), ("Bob", 3)] == [5, 3]
-- lookups 0 [(3, 'a'), (0, 'b'), (0, 'c'), (2, 'd'), (0, 'e')] == "bce"
-- lookups 0 [(1, True), (2, False)] == []
-- lookups 3 [] == []
lookups :: Eq a => a -> [(a, b)] -> [b]
lookups e xs = map snd $ filter (\(a,b) -> a == e) xs

-- 7
-- findIndices' (== 'l') "Hello world" == [2, 3, 9]
-- findIndices' (< 0) [-1, 0, 1, -2, 3] == [0, 3]
-- findIndices' (< 0) [1, 2, 3] == []
-- findIndices' (\_ -> True) [] == []
findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' p xs = map snd $ filter (\(a,b) -> p a) $ zip xs [0..]