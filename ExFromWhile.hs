module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
-- fromWhile n p xs = takeWhile p (drop n xs)
fromWhile n p    = takeWhile p . drop n

fromFor :: Int -> Int -> [a] -> [a]
-- fromFor n m xs = take (m - n) (drop n xs)
fromFor n m    = take (m - n) . drop n

fromTo :: Int -> Int -> [a] -> [a]
-- fromTo n m xs = take (m - n + 1) (drop n xs)
fromTo n m    = take (m - n + 1) . drop n

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
-- fromToThat n m p xs = fromWhile n p (take m xs)
fromToThat n m p    = fromWhile n p . take m
