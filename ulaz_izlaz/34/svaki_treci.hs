-- Napisati program koji prepisuje svaki treći karakter datoteke ulaz.txt u datoteku izlaz.txt. 
-- U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)

main :: IO ()
main = do
    postoji <- doesFileExist "ulaz.txt"
    case postoji of
        True -> do
            sadrzaj <- readFile "ulaz.txt"
            let filtrirano = svakiTreci sadrzaj
            writeFile "izlaz.txt" (filtrirano ++ "\n")
        False -> do
            putStrLn ("Greska: neuspesno otvaranje datoteke ulaz.txt")
            
svakiTreci :: String -> String
svakiTreci (x:_:_:xs) = x : svakiTreci xs
svakiTreci [x] = [x]
svakiTreci (x:_) = [x]
svakiTreci [] = []
