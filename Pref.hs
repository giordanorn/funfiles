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
    )

sum :: Num a => [a] -> a
sum xs = foldr (+) 0 xs
--sum [] = 0
--sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product xs = foldr (*) 1 xs

length :: Integral i => [a] -> i
length xs = foldr (\x acc -> 1 + acc) 0 xs

concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

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

