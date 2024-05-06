import Data.Char (toUpper)
{-
Homework 7 - May 2024
Neptun Code: OPXZ1E
Student: Helio Nogueira Cardoso
-}

-- 1
-- applyN 5 (+ 1) 0 == 5
-- applyN 5 (* 2) 1 == 32
-- applyN 2 ("hello" ++) "world" == "hellohelloworld"
-- applyN 0 (* 2) 1 == 1
applyN :: Int -> (a -> a) -> a -> a
--applyN n f =  last . take (n + 1) . iterate f
-- applyN n f e = snd $ until (\(i,_) -> i == n) (\(i,x) -> (i+1, f x)) (0,e)
applyN n f = (!! n) . iterate f

-- 2
type Line = String
type File = [Line]

testFile0 :: File
testFile0 = ["asd  qwe", "-- Foo", "", "\thello world "]

-- map countWordsInLine testFile0 == [2, 2, 0, 2]
countWordsInLine :: Line -> Int
countWordsInLine = length . words

-- 3
-- countWords testFile0 == 6
countWords :: File -> Int
countWords = sum . map countWordsInLine

-- 4
-- countChars testFile0 == 27
countChars :: File -> Int
countChars = sum . map length

-- 5
-- capitalizeWord "hello" == "Hello"
-- capitalizeWord "" == ""
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

-- 6
-- map capitalizeWordsInLine testFile0 == ["Asd Qwe", "-- Foo", "", "Hello World"]
capitalizeWordsInLine :: Line -> Line
capitalizeWordsInLine = unwords . map capitalizeWord . words

-- 7
-- map isComment testFile0 == [False, True, False, False]
isComment :: Line -> Bool
isComment (x:y:_) = x == '-' && y == '-'
isComment (_:_) = False
isComment [] = False

-- 8
-- dropComments testFile0 == ["asd  qwe", "", "\thello world "]
dropComments :: File -> File
dropComments = filter $ not . isComment

-- 9
-- numberLines testFile0 == ["1: asd  qwe", "2: -- Foo", "3: ", "4: \thello world "]
numberLines :: File -> File
numberLines = zipWith (\i l -> show i ++ ": " ++ l) [1..]

-- 10
-- replaceTab 3 '\t' == "   "
-- replaceTab 3 'a'  == "a"
replaceTab :: Int -> Char -> String
replaceTab n '\t' = replicate n ' '
replaceTab _ c = [c]

-- 11
-- replaceTabs 5 testFile0 == ["asd  qwe", "-- Foo", "", "     hello world "]
replaceTabs :: Int -> File -> File
replaceTabs n = map $ concatMap $ replaceTab n

-- 12
-- fixpoint (\x -> if x <= 0 then 0 else x - 1) 20 == 0
-- Heron's method for computing the square root of 2:
-- fixpoint (\x -> 1/2 * (x + 2/x)) 1 == 1.414213562373095
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = until (\x -> f x == x) f

-- 13
-- filter' even [1, 2, 3, 4, 5, 6] == [2, 4, 6]
-- filter' even [] == []
-- take 5 (filter' odd [1..]) == [1, 3, 5, 7, 9]
filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p = foldr h []
--     where
--         h x xs
--             | p x       = x : xs
--             | otherwise = xs
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- 14
-- reverse' "hello" == "olleh"
-- reverse' [1, 2, 3] == [3, 2, 1]
-- reverse' [] == []
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []