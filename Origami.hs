module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , sum , product
    , length
    , concat
    , filter
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    , scanr , scanl
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (#) v []     = v
foldr (#) v (x:xs) = x # foldr (#) v xs

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl (#) v []     = v
foldl (#) v (x:xs) = foldl (#) v xs # x

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 (#) []     = error "empty list"
foldr1 (#) [x]    = x
foldr1 (#) (x:xs) = x # foldr1 (#) xs

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 (#) []     = error "empty list"
foldl1 (#) [x]    = x
foldl1 (#) (x:xs) = foldl1 (#) xs # x


--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = undefined
-- scanl (#) v []     = [v]
-- scanl (#) v [x]    = [v, v # x]
-- scanl (#) v (x:y:xs) = scanl (#) v xs ++ [x]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr (#) v []     = [v]
-- scanr (#) v [x]    = [x # v, v]
-- scanr (#) v [y,x]  = [y # (x # v), x # v, v]
scanr (#) v (x:xs) = x # y : ys
        where (y:ys) = scanr (#) v xs 

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (||) False . map p

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (&&) True . map p

and :: [Bool] -> Bool
and = undefined

or :: [Bool] -> Bool
or = undefined

length :: Integral i => [a] -> i
length = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

reverse :: [a] -> [a]
reverse = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = undefined

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined

