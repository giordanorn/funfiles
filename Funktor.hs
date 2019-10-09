module Funktor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const

  -- ALGEBRAIC LAWS

instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

-- what about Either?
instance Funktor (Either c) where
    -- fmap :: (a -> b) -> Either c a -> Either c b
    fmap f (Left x)  = Left x
    fmap f (Right x) = Right (f x)

-- what about pairs?
instance Funktor ((,) c) where
    -- fmap :: (a -> b) -> (c,a) -> (c,b)
    fmap f (x,y)  = (x,f y)

-- what about functions?
instance Funktor ((->) c) where
    -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
    fmap = (.)

-- what about Trees?
data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

instance Funktor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x)     = Leaf (f x)
    fmap f (Node x t u) = Node (f x) (fmap f t) (fmap f u)

-- ...define Functor instances of as many * -> * things as you can think of!
