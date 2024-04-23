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