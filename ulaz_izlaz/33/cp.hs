-- Napisati program koji prepisuje sadržaj datoteke ulaz.txt u datoteku izlaz.txt karakter po karakter. 
-- U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)

main :: IO ()
main = do
    postoji <- doesFileExist "ulaz.txt"
    case postoji of
        True -> do
            sadrzaj <- readFile "ulaz.txt"
            writeFile "izlaz.txt" sadrzaj
        False -> do
            putStrLn ("Greska: neuspesno otvaranje datoteke ulaz.txt")
