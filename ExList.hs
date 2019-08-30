module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head []    = error "empty list"
head (x:_) = x

tail :: [a] -> [a]
tail []     = error "empty list"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (x:xs) = length xs + 1

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = sum xs + x

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = product xs * x

append :: a -> [a] -> [a]
append x []     = [x]
append x (y:ys) = y : append x ys

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = append x (reverse xs)

(++) :: [a] -> [a] -> [a]
[]     ++ l = l
(x:xs) ++ l = x : (xs ++ l)


-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x []     = [x]
snoc x (y:ys) = y : (snoc x ys)

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

-- minimum
minimum :: [a] -> a
minimum []     = error "empty list"
minimum (x:xs) = undefined

-- maximum

-- take
take :: Int -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x:xs)
    | n >= 0    = x : take (n - 1) xs
    | otherwise = error "negative number"

-- drop
drop :: Int -> [a] -> [a]
drop 0 l      = l
drop _ []     = []
drop n (x:xs) = drop (n - 1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p []  = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = xs

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []  = []
dropWhile p (x:xs)
    | p x       = dropWhile p xs
    | otherwise = x : xs

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

