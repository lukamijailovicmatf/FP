-- Napisati program koji prebrojava mala slova u datoteci podaci.txt i dobijeni rezultat ispisuje na standardni izlaz. 
-- U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)
import Data.Char (isLower)

main :: IO ()
main = do
    postoji <- doesFileExist "podaci.txt"
    case postoji of
        True -> do
            sadrzaj <- readFile "podaci.txt"
            let brojMalih = length (filter isLower sadrzaj)
            putStrLn ("Broj malih slova je: " ++ show brojMalih)
        False -> do
            putStrLn ("Greska: neuspesno otvaranje datoteke podaci.txt")
