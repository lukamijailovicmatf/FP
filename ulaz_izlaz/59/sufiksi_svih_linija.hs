-- Napisati program koji prebrojava koliko se linija datoteke ulaz.txt završava niskom s koja se učitava sa standardnog ulaza. 
-- Pretpostaviti da je maksimalna dužina linije 80 karaktera, a niske s 20 karaktera.

module Main where

import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Data.List (isSuffixOf)

main :: IO ()
main = do
    let imeDatoteke = "ulaz.txt"
    postoji <- doesFileExist imeDatoteke
    if not postoji
        then putStrLn ("Greska: neuspesno otvaranje ulazne datoteke.")
        else do
            putStr ("Unesite nisku s: ")
            hFlush stdout
            s <- getLine
            sadrzaj <- readFile imeDatoteke
            let sveLinije = lines sadrzaj
            let odgovarajuceLinije = filter (\linija -> s `isSuffixOf` linija) sveLinije
                broj = length odgovarajuceLinije
            putStrLn ("Broj linija: " ++ show broj)
