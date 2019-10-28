module Hangman0 where

import System.IO
import Data.Char

hangman :: IO ()
-- {{{ spoiler 
hangman = do putStrLn "Choose word:"
             word <- getSecretLine
             putStrLn "Try to guess it:"
             play word
-- }}}

getSecretLine :: IO String
-- {{{ spoiler 
getSecretLine = do 
    hSetEcho stdin False
    x <- getLine
    hSetEcho stdin True
    return x

-- }}}

prompt :: IO ()
prompt = do
    putStr "? "

play :: String -> IO ()
-- {{{ spoiler 
play word =
    do prompt
       guess <- getLine
       if guess == word
          then putStrLn "You got it!"
          else do putStrLn "Nope..."
                  play word
-- }}}

