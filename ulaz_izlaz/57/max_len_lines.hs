-- Napisati program koji u datoteci čije se ime navodi kao argument komandne linije određuje liniju maksimalne 
-- dužine i ispisuje je na standardni izlaz. Ukoliko ima više takvih linija, ispisati onu koja je leksikografski prva.
-- Pretpostaviti da je maksimalna dužina linije 80 karaktera.

module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)

izaberiBolju :: String -> String -> String
izaberiBolju l1 l2
    | length l1 > length l2 = l1
    | length l1 < length l2 = l2
    | otherwise = min l1 l2

main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let imeDatoteke = head argumenti
            postoji <- doesFileExist imeDatoteke
            if not postoji
                then putStrLn ("Greska: neuspesno otvaranje ulazne datoteke.")
                else do
                    sadrzaj <- readFile imeDatoteke
                    let sveLinije = lines sadrzaj
                    if null sveLinije
                        then putStrLn ("Greska: prazna datoteka.")
                        else do
                            let najbolja = foldl1 izaberiBolju sveLinije
                            putStrLn najbolja
