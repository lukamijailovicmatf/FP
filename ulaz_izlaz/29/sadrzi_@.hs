-- Napisati program koji ispisuje argumente komandne linije koji počinju karakterom @.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let filtrirani = filter (\argument -> head argument == '@') argumenti
    putStrLn ("Argumenti koji pocinu sa @:")
    putStrLn (unwords filtrirani)
