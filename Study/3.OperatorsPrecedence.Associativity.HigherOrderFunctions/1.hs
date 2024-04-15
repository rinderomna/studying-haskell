{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

{-
Mon 15 Apr 2024
* Operators Precedence and Associativity, Arithmetic (Lecture 9)

- Functions can be applied in either prefix or infix form
- prefix: mod n 2 ("default")
- infix: x * 2

- <->:
mod n 2
n `mod` 2

2 * 4
(*) 2 4
-}

(%?) :: Int -> Int -> Int
a %? b = a `mod` b

-- ghci> 10 %? 4
-- 2

{-
- Precedence
2 * 3 ^ 4

-------------------------------------------------------------------------------
ghci> :i (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in ‘GHC.Real’
infixr 8 ^ {>> 8 is stronger than 7} {Right associative}
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
ghci> :i (*)
type Num :: * -> Constraint
class Num a where
  ...
  (*) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 7 * {>> 7 is weaker than 8} {Left associative}
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
ghci> 2*3^4
162
ghci> 2*(3^4)
162
ghci> (2*3)^4
1296
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
ghci> 2^3^4
2417851639229258349412352
ghci> 2^(3^4)
2417851639229258349412352
ghci> (2^3)^4
4096
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
ghci> 16 / 2 / 2
4.0
ghci> (16 / 2) / 2
4.0
ghci> 16 / (2 / 2)
16.0
-------------------------------------------------------------------------------

- left associative -> (/)   {infixl}
- right associative -> (^)  {infixr}
- not associative -> (==)   {infix}

-------------------------------------------------------------------------------
ghci> 2 < 3 < 4

<interactive>:21:1: error:
    Precedence parsing error
        cannot mix ‘<’ [infix 4] and ‘<’ [infix 4] in the same infix expression
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
ghci> 2 < 3 && 3 < 4
True
-------------------------------------------------------------------------------
- Function applications have the highest precedence

-------------------------------------------------------------------------------
ghci> 12 + 3 ^ 2
21
ghci> (+) 12 3 ^ 2
225
ghci> div 12 3 ^ 2
16
ghci> 12 `div` 3 ^ 2
1
-------------------------------------------------------------------------------

-}

{-
* Higher Order Functions; Currying; Anonymous Functions
- Take one or more functions as arguments
- Return a function as result

-}

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- more restrictive than needed:
foldR :: a -> (a -> a -> a) -> [a] -> a
foldR e _ []    = e
foldR e f (x:xs)= x `f` foldR e f xs

{-
ghci> foldR 0 (+) [1..5]
15
ghci> foldR 1 (*) [1..5]
120
ghci> sum'' = foldR 0 (+)
ghci> product'' = foldR 1 (*)
ghci> sum'' [1..5]
15
ghci> product'' [1..5]
120
ghci> concat'' = foldR [] (++)
ghci> concat'' [[1,2], [3,4]]
[1,2,3,4]
-}

-- more general:
foldR' :: (a -> b -> b) -> b -> [a] -> b
foldR' _ e []    = e
foldR' f e (x:xs)= x `f` foldR' f e xs

-- example with different types:
{-
ghci> :t (:)
(:) :: a -> [a] -> [a]
-}

(+:+) :: [a] -> [a] -> [a]
(+:+) ls ks = foldR' (:) ks ls

-------------------------------------------------------------------------------
length' :: [a] -> Integer
length' = foldR' (\_ acc -> 1 + acc) 0

