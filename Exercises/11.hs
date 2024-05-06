-- 1
data Day = 
    Monday    | 
    Tuesday   |
    Wednesday |
    Thursday  |
    Friday    |
    Saturday  |
    Sunday
        deriving (Show, Eq, Ord, Enum)

-- 2
isFirstDayOfWeek :: Day -> Bool
isFirstDayOfWeek Monday = True
isFirstDayOfWeek _ = False

-- 3
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _ = False

-- 4
nextDay :: Day -> Day
nextDay = toEnum. (`mod` 7) . (+1) . fromEnum 

-- next :: Day -> Day
-- next x = toEnum ((fromEnum x + 1) `mod` 7)

-- 5. [TODO]

-- 6.
type Hour = Int
type Minute = Int
data Time = T Hour Minute

instance Show Time where
    show :: Time -> String
    show = showTime

showTime :: Time -> String
showTime (T h m) = show h ++ "." ++ show m

class Show' a where
    show' :: a -> String

instance Show' Int where
    show' :: Int -> String
    show' = show

-- show' (5 :: Int)

instance Show' Bool where
    show' :: Bool -> String
    show' True = "yes"
    show' False = "no"

-- show' True

-- 7
eqTime :: Time -> Time -> Bool
eqTime (T h1 m1) (T h2 m2) = h1 == h2 && m1 == m2

-- instance Eq Time where
--     (==) :: Time -> Time -> Bool
--     (==) = eqTime

instance Eq Time where
    (==) :: Time -> Time -> Bool
    T h1 m1 == T h2 m2 = h1 == h2 && m1 == m2

-- 8
isEarlier :: Time -> Time -> Bool
-- isEarlier (T h1 m1) (T h2 m2)
--     | h1 == h2 = m1 < m2
--     | otherwise = h1 < h2
isEarlier (T h1 m1) (T h2 m2) = (h1 == h2) && (m1 < m2) || (h1 < h2)

instance Ord Time where
    (<=) :: Time -> Time -> Bool
    t1 <= t2 = isEarlier t1 t2 || t1 == t2 

{-
class Eq a where
    (==) :: a -> a -> Bool 
-}

{-
class Eq a => Ord a where
    (<=) :: a -> a -> Bool
-}

gtEq :: Ord a => a -> a -> Bool
gtEq x y = (x > y) || (x == y)

isBetween :: Time -> Time -> Time -> Bool
isBetween t1 t2 t3 = t1 < t2 && t2 < t3 || t1 < t2 && t2 < t3

isBetween' :: Ord a => a -> a -> a -> Bool
isBetween' t1 t2 t3 = t1 < t2 && t2 < t3 || t1 < t2 && t2 < t3

-- 9
time :: Hour -> Minute -> Time
time h m
    | not (0 <= h && h <= 24) = error ("time: invalid hour: " ++ show h)
    | not (0 <= m && m <  60) = error ("time: invalid minute: " ++ show m)
    | otherwise = T h m

-- 10
data USTime = AM Hour Minute | PM Hour Minute
-- deriving (Show)

instance Eq USTime where
    (==) :: USTime -> USTime -> Bool
    AM h1 m1 == AM h2 m2 = h1 == h2 && m1 == m2
    PM h1 m1 == PM h2 m2 = h1 == h2 && m1 == m2
    _ == _ = False


-- 11
showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m  ++ " pm"

instance Show USTime where
    show :: USTime -> String
    show = showUSTime

-- 12
usTimeToTime :: USTime -> Time
usTimeToTime (AM h m)
    | h == 12 = T 0 m
    | otherwise = T h m
usTimeToTime (PM h m)
    | h == 12 = T 12 0
    | otherwise = T (12 + h) m

-- 13
timeToUSTime :: Time -> USTime
timeToUSTime (T h m)
    | h == 0 = AM 12 m
    | h < 12 = AM h m
    | h == 12 = PM 12 m
    | otherwise = PM (h - 12) m

-- Algebraic Data Types