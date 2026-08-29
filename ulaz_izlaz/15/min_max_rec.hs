-- Dato 
-- main :: IO()
-- main = undefined
-- i u komentarima 1.txt 2.txt sa nekim recima u njima.
-- Uraditi ucitavanje reci iz vise fajlova (./Main 1.txt 2.txt) i pronaci najkracu i najduzu rec od svih reci iz svih fajlova. Ispisati: 
-- min: najkraca_rec 
-- max: najduza_rec.

module Main where

import Data.List (minimumBy, maximumBy)
import System.Environment (getArgs)

porediReci :: String -> String -> Ordering
porediReci rec1 rec2 = compare (length rec1) (length rec2)

main :: IO ()
main = do
    putanje <- getArgs
    if null putanje then putStrLn ("Greska. Potrebno je proslediti barem jedan fajl.") else do
        sadrzaji <- mapM readFile putanje
        let sveReci = concatMap words sadrzaji
        if null sveReci then putStrLn ("Greska. Prosledjeni fajlovi ne sadrze nijednu rec.") else do
            let najkraca = minimumBy porediReci sveReci
            let najduza = maximumBy porediReci sveReci
            putStrLn ("min: " ++ najkraca)
            putStrLn ("max: " ++ najduza)
