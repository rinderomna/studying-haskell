{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Compat.Lens (_1)
{-# HLINT ignore "Use foldr" #-}

-- 1
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 2
last' :: [Integer] -> Integer
last' [x] = x
last' (_:xs) = last' xs

-- 3
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- 4
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

-- 5
replicate' :: Int -> Char -> String
replicate' 0 _ = ""
replicate' n c = c : replicate' (n - 1) c

-- 5 (Bonus: Replicate Strings)
replicateString :: Int -> String -> String
replicateString 0 _ = ""
replicateString n s = s ++ replicateString (n - 1) s

-- 6
format :: Int -> String -> String
format n s 
    | n <= l = s
    | otherwise = format n (' ':s)
    where 
        l = length s

format' :: Int -> String -> String
format' n s 
    | n <= l = s
    | otherwise = ' ' : format (n - 1) s
    where 
        l = length s

format'' :: Int -> String -> String -- more efficient!
format'' n s 
    | n <= l = s
    | otherwise = replicate (l - n) ' ' ++ s
    where 
        l = length s

-- 7
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y:insert x ys

-- 8 (Insertion Sort)
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insert x (sort xs)

-- 9 
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 10
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
    where
        l = length xs
        halfLen = l `div` 2
        ys = take halfLen xs
        zs = drop halfLen xs -- New! function 'drop'

--- Better, using auxilary split function:
split :: [Int] -> ([Int], [Int])
split [] = ([], [])
split [x] = ([x], [])
split (y:z:xs) = (y:ys, z:zs)
    where   
        (ys, zs) = split xs

mergeSort' :: [Int] -> [Int]
mergeSort' [] = []
mergeSort' [x] = [x]
mergeSort' xs = merge (mergeSort ys) (mergeSort zs)
    where
        l = length xs
        halfLen = l `div` 2
        (ys, zs) = split xs

-- 11
breakOn :: Char -> String -> (String, String)
breakOn _ "" = ("", "")
breakOn c (x:xs) 
    | c == x = ("", c:xs)
    | otherwise = (x:ys, zs)
    where
        ~(ys, zs) = breakOn c xs -- tilde to garantee it is lazy!

-- 12 -- Getting Harder! Study this at home!
--Updated prompt:
-- splitOn '/' "" = [""]
-- splitOn '/' "//" = ["","",""]
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c (x:xs) 
    | c == x = "":ys:yss
    | otherwise = (x:ys):yss
    where 
        ys:yss = splitOn c xs

-- 13
csv :: String -> [[String]]
csv s = [splitOn ',' row | row <- splitOn '\n' s]

csv' :: String -> [[String]]
csv' s = [splitOn ',' row | row <- lines s] -- New! function 'lines'!