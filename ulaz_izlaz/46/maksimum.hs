-- Napisati program koji na standardni izlaz ispisuje maksimum brojeva iz datoteke brojevi.txt.

module Main where

import Data.List (maximum)

main :: IO ()
main = do
    sadrzaj <- readFile "brojevi.txt"
    let brojevi = map read (words sadrzaj) :: [Double]
    let maksimum = maximum brojevi
    putStrLn ("Najveci broj je: " ++ show maksimum)
