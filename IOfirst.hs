module IOkit where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

import System.IO
    ( hSetEcho
    , hGetEcho
    , stdin
    , stdout
    )

getLine :: IO String
-- {{{ spoiler 
getLine = do
    c <- getChar
    if isEOL c
       then return ""
       else do l <- getLine
               return (c:l)
-- }}}

isEOL :: Char -> Bool
-- {{{ spoiler 
isEOL = (== '\n')
-- }}}

getInt :: IO Int
-- {{{ spoiler 
getInt =
    do s <- getLine
       return $ read s
-- }}}
{-
-- {{{ spoiler 
getInt = iomap read getLine
-- }}}
-}

{- alternative definitions

-- first definition:

-- also valid one-liner:
-- {{{ spoiler 
getInt = getLine >>= (return . read)
-- }}}

-}

getSafeInt :: IO (Maybe Int)
-- {{{ spoiler 
getSafeInt =
  do s <- getLine
     let parsed = reads s :: [(Int, String)]
     case parsed of
       [(n, "")] -> return $ Just n
       _         -> return Nothing
-- }}}

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
(>>) = undefined

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void = undefined

-- pauses till the user presses any normal key
pause :: IO ()
pause = undefined

skip :: IO ()
skip = undefined

newline :: IO ()
newline = undefined

-- exercise: define it as a foldr
putStr :: String -> IO ()
putStr = undefined

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize = undefined

putStrLn :: String -> IO ()
putStrLn = undefined

putCharLn :: Char -> IO ()
putCharLn = undefined

