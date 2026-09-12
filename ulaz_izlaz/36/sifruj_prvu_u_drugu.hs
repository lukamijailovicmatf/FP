-- Napisati program koji za dve datoteke čija se imena unose sa standardnog ulaza, radi sledeće:
-- • za svaku cifru u prvoj datoteci, u drugu datoteku upisuje 0
-- • za svako slovo u prvoj datoteci, u drugu datoteku upisuje 1
-- • za sve ostale karaktere u prvoj datoteci, u drugu datoteku upisuje 2
-- Pretpostaviti da je maksimalna dužina naziva datoteka 20 karaktera. U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)
import Data.Char (isDigit, isAlpha)

main :: IO ()
main = do
    putStrLn ("Unesite ime prve datoteke:")
    datoteka1 <- getLine
    putStrLn ("Unesite ime druge datoteke:")
    datoteka2 <- getLine
    postoji <- doesFileExist datoteka1
    case postoji of
        True -> do
            sadrzaj <- readFile datoteka1
            let sifrovano = map transformisiKarakter sadrzaj
            writeFile datoteka2 (sifrovano ++ "\n")
        False -> do
            putStrLn ("Greska: neuspesno otvaranje prve datoteke")
            
transformisiKarakter :: Char -> Char
transformisiKarakter c
    | isDigit c = '0'
    | isAlpha c = '1'
    | otherwise = '2'
