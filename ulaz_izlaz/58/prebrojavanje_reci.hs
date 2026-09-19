-- U datoteci čije se ime navodi kao prvi argument komandne linije navedena je reč r i niz linija. 
-- Napisati program koji u datoteku čije se ime navodi kao drugi argument komandne linije upisuje sve linije prve datoteke u
-- kojima se reč r pojavljuje bar n puta gde je n pozitivan ceo broj koji se unosi sa standardnog ulaza. Prilikom prebrojavanja, 
-- računaju se i samostalna pojavljivanja reči r i pojavljivanja u okviru neke druge reči. Ispis treba da bude u formatu 
-- broj_pojavljivanja:linija. Pretpostaviti da je maksimalna dužina reči 100 karaktera, a linije 500 karaktera.

module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (isPrefixOf)
import System.IO (hFlush, stdout)

prebroj :: String -> String -> Int
prebroj [] _ = 0
prebroj _ [] = 0
prebroj rec linija@(x:xs)
    | rec `isPrefixOf` linija = 1 + prebroj rec (drop (length rec) linija)
    | otherwise = prebroj rec xs

main :: IO ()
main = do
    argumenti <- getArgs
    if length argumenti /= 2
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let ulaznaDatoteka = argumenti !! 0
                izlaznaDatoteka = argumenti !! 1
            postoji <- doesFileExist ulaznaDatoteka
            if not postoji
                then putStrLn ("Greska: neuspesno otvaranje ulazne datoteke.")
                else do
                    putStr ("Unesite broj n: ")
                    hFlush stdout
                    unosN <- getLine
                    let n = read unosN :: Int
                    sadrzaj <- readFile ulaznaDatoteka
                    let sveLinije = lines sadrzaj
                    if null sveLinije
                        then writeFile izlaznaDatoteka ""
                        else do
                            let rec = head sveLinije
                                linijeZaPretragu = tail sveLinije
                            let rezultat = [show brojac ++ ": " ++ linija | linija <- linijeZaPretragu, let brojac = prebroj rec linija, brojac >= n]
                            writeFile izlaznaDatoteka (unlines rezultat)
