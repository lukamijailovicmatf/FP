-- Napisati program koji šifruje sadržaj datoteke podaci.txt tako što svako slovo ciklično zamenjuje njegovim prethodnikom suprotne veličine
-- i upisuje u datoteku sifra.txt. Na primer, slovo b se zamenjuje slovom A, slovo B slovom a, slovo a slovom Z, slovo A slovom z, itd. 
-- Ostali karakteri ostaju nepromenjeni. U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)
import Data.Char (isLower, isUpper, toLower, toUpper)

main :: IO ()
main = do
    postoji <- doesFileExist "podaci.txt"
    case postoji of
        True -> do
            sadrzaj <- readFile "podaci.txt"
            let sifrovano = map sifrujKarakter sadrzaj
            writeFile "sifra.txt" sifrovano
        False -> do
            putStrLn ("Greska: neuspesno otvaranje datoteke podaci.txt")
            
sifrujKarakter :: Char -> Char
sifrujKarakter 'a' = 'Z'
sifrujKarakter 'A' = 'z'
sifrujKarakter c
    | isLower c = toUpper (pred c)
    | isUpper c = toLower (pred c)
    | otherwise = c
