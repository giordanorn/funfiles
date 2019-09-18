module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes []             = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x  : xs) = x : catMaybes xs

fromJust :: Maybe a -> a
fromJust Nothing  = error "Nothing"
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_)  = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) = case f x of
    Nothing -> mapMaybe f xs 
    Just y  -> y : mapMaybe f xs 

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just y) = f y

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith [] _                  = []
tryToModifyWith _ []                  = []
tryToModifyWith (Nothing : xs) ys     = tryToModifyWith xs ys
tryToModifyWith (Just f  : xs) (y:ys) = f y : tryToModifyWith xs ys

tryToModifyWith' :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith' = map . compose . catMaybes

compose :: [(a -> a)] -> a -> a
compose []  = id 
compose (f:fs) = f . compose fs

