module ExDrunk
    ( atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    , replicate
    ) where

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
myDrop :: Integral i => i -> [a] -> [a]
myDrop 0 xs     = xs
myDrop n (x:xs) = myDrop (n-1) xs
myDrop n []     = []

myTake :: Integral i => i -> [a] -> [a]
myTake  0 xs     = []
myTake n (x:xs) = x:myTake (n-1) xs
myTake n []     = []

atIndices :: Integral i => [i] -> [a] -> [a]
atIndices [] _  = []
atIndices _  [] = []
atIndices (n:ns) xs = (head $ myDrop n xs):atIndices ns xs

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Integral i => i -> [a] -> [a]
everyOther 0 xs = xs
everyOther _ [] = []
everyOther n (xs) = (head $ myTake (n-1) xs):everyOther n (myDrop n xs)

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint = undefined

-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
nplicate :: Integral i => i -> a -> [a]
nplicate 1 x = [x]
nplicate n x = x:nplicate (n-1) x

stretch :: Integral i => i -> [a] -> [a]
stretch n = concat . map (nplicate n)

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Integral i => i -> [a] -> [a]
drunk = undefined

