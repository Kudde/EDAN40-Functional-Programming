module Utilities where

-- map2 ((+2), (*3)) (1 , 2) = (3, 6)
-- Applies (f1 to x1) and (f2 to x2) and puts the result in a touple
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- mmap (++"!") Nothing = Nothing
-- mmap (++"!") (Just "wisdom") = Just "wisdom!"
-- mmap (*2) (Just 3) = Just 6
-- Applies f if something else nothing
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- if a is Nothing then b
-- if a is Just something then Just something
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- if (f x) returns a Just something, pick that value
-- if it returns Nothing pick x
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- if f(x) = x then return x
-- else call f(f(x))
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- xs -> list
-- u  -> number? percentage?
-- list !! index -> value at index
-- fromIntegral takes an Integral and turns it into Num
-- floor 1.7 -> 1 etc
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
