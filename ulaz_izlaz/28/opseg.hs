-- Napisati program koji na osnovu broja n koji se zadaje kao argument komandne linije, ispisuje cele brojeve iz intervala [−n,n].

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let n = read (head argumenti) :: Int
    let granica = abs n
    let brojevi = [-granica..granica]
    putStrLn (unwords (map show brojevi))
