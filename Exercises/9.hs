-- 1
-- map' (\n -> n + 2) [] == []
-- map' (\n -> n + 2) [2,3,4] == [4,5,6]
-- map' even [2,3,4] == [True, False, True]
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

-- 2
-- filter'' (\n -> n > 5) [] == []
-- filter'' (\n -> n > 5) [1,2,5,6,0] == [6]
-- filter'' even [1,2,5,6,0] == [2,6,0]
-- filter'' (elem 0) [[5,6],[4,1,2,0],[0,5]] == [[4,1,2,0],[0,5]]
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [e | e <- xs, f e]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []
filter'' f (x:xs)
    | f x = x : res
    | otherwise = res
    where
        res = filter'' f xs

-- 3
-- upperToLower "" == ""
-- upperToLower "Hello World!" == "hw"
-- upperToLower "haSKell" == "sk"
upperToLower :: String -> String
upperToLower s = map' (\x -> toEnum (fromEnum x - diff)) upperOnly
    where
        upper :: Enum a => a -> Bool
        upper x = pos >= fromEnum 'A' && pos <= fromEnum 'Z'
            where
                pos = fromEnum x

        upperOnly = filter upper s
        diff = fromEnum 'A' - fromEnum 'a'

upperToLower' :: String -> String
upperToLower' s = [toEnum (fromEnum e - diff) | e <- s, e `elem` ['A'..'Z']]
    where
        diff = fromEnum 'A' - fromEnum 'a'
-- 4
-- all' (\n -> n > 0) []
-- all' (\n -> n > 0) [1,2,9,6]
-- not (all' (\n -> n > 0) [1,2,-4,9,6])
-- not (all' even [4,6,8,2,3,0])
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

-- 5
-- not (any' (\n -> n == 2) [])
-- not (any' (\n -> n == 2) [1,5,9,0,3])
-- any' (\n -> n == 2) [1,5,9,2,0,3,2]
-- any' even [1,2,5,9,2,0,3,2]
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

-- 6
-- not (hasLongLine "first\nsecond\nthird line")
-- hasLongLine "first\none fat line\nthird line"
hasLongLine :: String -> Bool
hasLongLine = any' (>= 3). map (length . words) . lines

hasLongLine' :: String -> Bool
hasLongLine' s = any' (\x -> length x >= 3) [words l | l <- lines s]

-- 7
-- elem' 'n' "Finn"
-- elem' 'H' "Harry"
-- not (elem' 'h' "Harry")
-- elem' True [False, False, True]
-- not (elem' True [False, False, False])
elem' :: Eq a => a -> [a] -> Bool
elem' e xs = any' (`elem` xs) xs

-- 8
-- hasAny "abc" "I like Haskell"
-- hasAny [5,9] [4, 3, 2, 0, 9]
-- not (hasAny ["haskell", "python"] ["c", "java"])
hasAny :: Eq a => [a] -> [a] -> Bool
hasAny ls xs = any' id (map (`elem'` xs) ls)
--hasAny ls xs = any' (uncurry (==)) [(x, y) | x <-ls, y <- xs]

-- 9
-- takeWhile' (\n -> n > 5) [] == []
-- takeWhile' (\n -> n > 5) [6,7,9,5,2,1] == [6,7,9]
-- takeWhile' odd [6,7,9,5,2,1] == []
-- takeWhile' odd [7,9,5,2,1] == [7,9,5]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

-- 10
-- dropWhile' (\n -> n > 5) [] == []
-- dropWhile' (\n -> n > 5) [6,7,9,5,2,1] == [5,2,1]
-- dropWhile' odd [6,7,9,5,2,1] == [6,7,9,5,2,1]
-- dropWhile' odd [7,9,5,2,1] == [2,1]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

-- 11
-- dropWord "" == ""
-- dropWord " tree " == " tree "
-- dropWord "apple tree " == " tree "
-- dropWord "appletree" == ""
dropWord :: String -> String
dropWord = dropWhile' (/= ' ')

-- 12