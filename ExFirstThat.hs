module ExFirstThat where

-- Q: Who is the first of these that has the property p?
-- Possible answers
-- * Nobody
-- * Just that guy
firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat = undefined

-- Q: What do you have to say about the first of these that has property p?  Do you like them or not?
-- Possible answers:
-- * Just a simple (yes/no)
-- * Nothing to say (cause nobody has this property)
isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat = undefined
