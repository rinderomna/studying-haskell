import Data.List (sort)

type String' = [Char]
type Name = String
type IntTuple = (Int, Int)
-- type Tuple = (a, a) [wrong]
-- f = x [bad; no context]
-- f x = x [good context]
type Tuple a = (a, a)

-- f :: Tuple a -> a
-- f (x, y) = 1 [bad]

f' :: Num b => Tuple a -> b
f' (x, y) = 1

type PredicateOn a = (a -> Bool)

-- ghci> :t even :: PredicateOn Int
--     even :: PredicateOn Int :: PredicateOn Int
-- ghci> :t even :: Integral a => PredicateOn a
--     even :: Integral a => PredicateOn a :: Integral a => PredicateOn a

--------------------------------------------------------------------------

data Bool' = False' | True'
--    deriving (Enum)
instance Show Bool' where
    show False' = "Nooo!!!"
    show True'  = "Yess!!!"

instance Eq Bool' where
    False' == False' = True
    True'  == True'  = True
    _ == _ = False

instance Ord Bool' where
    False' <= True'  = True
    True'  <= False' = False
    _ <= _           = True

not' :: Bool' -> Bool'
not' False' = True'
not' True'  = False'

instance Num Bool' where
    False' + False' = False'
    False' + True' = True'
    True' + False' = True'
    True' + True' = False'

    (*) = (+)

    abs = id

    signum = id

    negate = not' 

    fromInteger 0 = False'
    fromInteger _ = True'

instance Enum Bool' where
    toEnum 0 = False'
    toEnum _ = True'

    fromEnum False' = 0
    fromEnum True'  = 1

    enumFrom x = map toEnum [fromEnum x .. 1]

------------------------------------------------------

-- data Point a = P2 {x :: a, y :: a} | P3 {x,y,z :: a} deriving (Show, Eq)
data Point a = P2 {x,y :: a}  | P3 {x,y,z :: a} deriving (Show, Eq)

-- shift :: (Int, Int) -> Int -> (Int, Int)
shift :: Point Int -> Int -> Point int
shift = undefined

m :: Point Int -> Int -> Point Int
m (P2 x y) i   = P3 (x*i) (y*i) y
m (P3 x y z) i = P2 (x*i) (z*i) 

{-
ghci> take 11 $ iterate (\p -> m p 2) (P3 1 2 3)
[
    P3 1    2   3,
    P2 2    6 ,
    P3 4    12  6,
    P2 8    12,
    P3 16   24  12,
    P2 32   24,
    P3 64   48  24,
    P2 128  48,
    P3 256  96  48,
    P2 512  96,
    P3 1024 192 96
]
-}

{-
ghci> p = P3 1 2 3
ghci> p {y = 4}
    P3 {x = 1, y = 4, z = 3}
-}