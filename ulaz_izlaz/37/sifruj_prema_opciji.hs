-- Sa standardnog ulaza učitavaju se imena dveju datoteka i jedan karakter koji označava opciju. 
-- Napisati program koji prepisuje sadržaj prve datoteke u drugu tako što u slučaju da je navedena opcija u, sva mala
-- slova zamenjuje velikim slovima, a u slučaju da je navedena opcija l, sva velika slova zamenjuje malim slovima. 
-- Pretpostaviti da je maksimalna dužina naziva datoteka 20 karaktera. U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)
import Data.Char (toUpper, toLower)

main :: IO ()
main = do
    putStrLn ("Unesite imena datoteka i opciju:")
    unos <- getLine
    case words unos of
        [dat1, dat2, opcija] -> 
            case opcija of
                "u" -> obradiDatoteku dat1 dat2 (map toUpper)
                "l" -> obradiDatoteku dat1 dat2 (map toLower)
                _ -> putStrLn ("Greska: neispravan unos")
        _ -> putStrLn ("Greska: neispravan unos")
        
obradiDatoteku :: FilePath -> FilePath -> (String -> String) -> IO ()
obradiDatoteku dat1 dat2 transformacija = do
    postoji <- doesFileExist dat1
    case postoji of
        True -> do
            sadrzaj <- readFile dat1
            writeFile dat2 (transformacija sadrzaj)
        False -> do
            putStrLn ("Greska: neuspesno otvaranje prve datoteke")
