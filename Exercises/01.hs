{-
Szumi

Email: szumi@inf.elte.hu
Teams: Xie Zong Pu

Consultation: Thursday 14:00-15:00
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

y :: Int
y = 2

x :: Integer
x = 1 + 1

dobrar :: Num a => a -> a
dobrar x = x + x

dobrar2 :: Integer -> Integer
dobrar2 = \x -> x + x

inc x = x + 1

pT a b c =
    (a ^ 2 + b ^ 2 == c ^ 2) ||
    (a ^ 2 + c ^ 2 == b ^ 2) ||
    (b ^ 2 + c ^ 2 == a ^ 2) 

y' :: Integer
y' = 2

-- [TODO]: do the whole first list again