{-# OPTIONS_GHC -Wno-deprecations #-}

data Result a = Ok a | Error
safeDiv :: Int -> Int -> Result Int
safeDiv x y
    | y /= 0 = Ok (x `div` y)
    | otherwise = Error

safeDiv' :: Int -> Int -> Maybe Int
safeDiv' x y
    | y /= 0 = Just (x `div` y)
    | otherwise = Nothing

fromMaybe' :: a -> Maybe a -> a
fromMaybe' def Nothing = def
fromMaybe' _ (Just x) = x

