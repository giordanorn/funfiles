module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (x:xs) = case x of
    Left x' -> x' : lefts xs
    Right _ -> lefts xs

rights :: [Either a b] -> [b]
rights [] = []
rights (x:xs) = case x of
    Right x' -> x' : rights xs
    Left _   -> rights xs

fromLeft :: a -> Either a b -> a
fromLeft = undefined

fromRight :: b -> Either a b -> b
fromRight = undefined

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = undefined

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = undefined
