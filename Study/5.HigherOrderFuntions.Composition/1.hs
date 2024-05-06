{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use span" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use curry" #-}
{-# HLINT ignore "Avoid lambda" #-}

import Data.List (foldl')
import Text.XHtml (base, h1)

-- Mon 29 Apr 2024 12:15

-- Review:
{-
ghci> (`take` [1,2,3, 13, 4, 1]) 4
    [1,2,3,13]
ghci> map (`take` [1,2,3, 13, 4, 1]) [2, 3, 4, 1]
    [[1,2],[1,2,3],[1,2,3,13],[1]]
-}

-- function span
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' pred ls = (takeWhile pred ls ,dropWhile pred ls)
-- ^ Bad implementation. For better, look last class notes

-- As-Pattern (@)

-- function iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f e = e : iterate' f (f e)

-----------------------------------------------------------
-- Today:

-- foldl
-- One more big difference between foldr and foldl:

-- Curry:
{-
ghci> :t curry
    curry :: ((a, b) -> c) -> a -> b -> c
ghci> (\(x,y) -> x + y) (3, 4)
    7
ghci> curry (\(x,y) -> x + y) 3 4
    7
-}

curry' :: ((a, b) -> c) -> a -> b -> c
--curry' f x y = f (x, y)
curry' f = \x y -> f (x, y)

{-
ghci> ls = [(2, [1,2,3]),(3, [5,4,3,2,1])]
ghci> map (uncurry take) ls
    [[1,2],[5,4,3]]
-}

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x y -> p x || y) False

any'' :: (a -> Bool) -> [a] -> Bool
any'' p = foldl (\e x -> e || p x) False

any''' :: (a -> Bool) -> [a] -> Bool
any''' p = foldl' (\e x -> e || p x) False -- Strict version

{-
ghci> any' even [1..]
    True
ghci> any'' even [1..]
    ^CInterrupted.
-}

----------------------------------------------------
-- composition and !pipeline

-- even . (+ 1)
-- odd = not . even     (parallelization, separated processes)

-- ($) precedence 0 !!!


{-
ghci> map ($ 10) [even, odd]
    [True,False]
-}

infixr 0 $$
($$) :: (a -> b) -> a -> b
($$) f = f

(...) :: (b -> c) -> (a -> b) -> a -> c
(...) f g x =  f (g x)

(....) :: (b -> c) -> (a -> b) -> (a -> c)
(....) f g = \x -> f (g x)

(.....) :: (b -> c) -> (a -> b) -> (a -> c)
(.....) f g = h
    where
        h x = f $ g x