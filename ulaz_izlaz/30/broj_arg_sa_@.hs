-- Napisati program koji ispisuje broj argumenata komandne linije koji sadrže karakter @

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let filtrirani = filter (\argument -> '@' `elem` argument) argumenti
    let broj = length filtrirani
    putStrLn ("Rezultat: " ++ show broj)
