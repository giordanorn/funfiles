module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft = undefined

isRight :: Either a b -> Bool
isRight = undefined

lefts :: [Either a b] -> [a]
lefts = undefined

rights :: [Either a b] -> [b]
rights = undefined

fromLeft :: a -> Either a b -> a
fromLeft = undefined

fromRight :: b -> Either a b -> b
fromRight = undefined

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = undefined

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = undefined