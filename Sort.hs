module Sort 
    ( sort
    , msort
    , qsort
    , isort
    ) where

-- default sort algorithm
sort :: Ord a => [a] -> [a]
sort = msort

--
-- merge sort
--

msort :: Ord a => [a] -> [a]
msort [] = []
msort [z] = [z]
msort zs = merge (msort xs) (msort ys)
    where
        (xs,ys) = halve zs

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve [x] = ([x],[])
halve (x:y:xs) = (x:lxs, y:rxs)
    where
        (lxs,rxs) = halve xs

-- ASSUMPTION: xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x:xs) ys'@(y:ys)
  | x <= y     = x : merge xs ys'
  | otherwise  = y : merge xs' ys


--
-- insertion sort
--

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- ASSUMPTIONS FOR insert w xs:
--   xs is sorted
insert :: Ord a => a -> [a] -> [a]
insert w []     = [w]
insert w (x:xs)
  | w < x      =  w:x:xs
  | otherwise  =  x : insert w xs


--
-- quick sort
--

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (w:xs) = qsort small ++ [w] ++ qsort large
    where
        small =  [ x | x <- xs, x <= w ]
        large =  [ x | x <- xs, x > w  ]
