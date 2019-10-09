module Pref where

import Prelude hiding
    ( sum
    , product
    , length
    , concat
    , filter
    , map
    , any
    , all
    , and
    , or
    , foldr
    )

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op v []     = v
foldr op v (x:xs) = x `op` (foldr op v xs)

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

length :: Integral i => [a] -> i
length = foldr (\x acc -> 1 + acc) 0



-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- scanl f x []     = []
-- scanl f x (y:ys) = 




{-
filter :: (a -> Bool) -> [a] -> [a] 
filter p xs = [x | x <- xs , p x]

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr (\x acc -> p x `or`  acc) False xs

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr (\x acc -> p x `and`  acc) True xs

and :: (Bool -> Bool -> Bool)
and True True = True
and _ _ = False

or :: (Bool -> Bool -> Bool)
or False False = False
or _ _ = True
-}

