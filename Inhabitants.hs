module Inhabitants where

-- Define *convergent* (not bottom-ish) inhabitants
-- of the following types.
-- For each type, if you can define more than one
-- inhabitant, define a few of them, using primes
-- to name the other inhabitants, for example:
-- f, f', f'', f''', f'''', etc.

cI :: a -> a
cI x = x

cK :: a -> b -> a
cK x y = x

cS :: (a -> b -> c) -> (a -> b) -> (a -> c)
cS f g x = f x (g x)

cB :: (b -> c) -> ((a -> b) -> (a -> c))
cB f g = f . g

cW :: (a -> (a -> b)) -> (a -> b)
cW f x = f x x 

pL :: ((a -> b) -> a) -> a
pL f = undefined
-- boo

p1 :: (a,b) -> a
p1 (x,y) = x

p2 :: (a,b) -> b
p2 (x,y) = y

p3 :: (a,b) -> (b,a)
p3 (x,y) = (y,x)

p4 :: Either a b -> b
p4 = undefined

p5 :: Either a b -> [a]
p5 = undefined

p6 :: Either a b -> (a,b)
p6 = undefined

p7 :: (a,b) -> Either a b
p7 = undefined

p8 :: (a,b) -> Either a b
p8 = undefined

p9 :: a -> b -> (a,a,b)
p9 = undefined

q1 :: a -> a -> a
q1 = undefined

q2 :: (a,a) -> (a,a)
q2 = undefined

q3 :: Either (a,b) (a,c) -> (a, Either b c)
q3 = undefined

l1 :: [a]
l1 = undefined

l2 :: [[a]]
l2 = undefined

l3 :: [a] -> [a]
l3 = undefined

l4 :: a -> [a]
l4 = undefined

l5 :: [a] -> a
l5 = undefined

l6 :: a -> [[a]]
l6 = undefined

