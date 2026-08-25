-- Procitati fajl tekst.txt, pronaci najduzu rec u njemu i u fajl najduza.txt upisati tu rec i njenu duzinu.

module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = do
    sadrzaj <- readFile "tekst.txt"
    let sveReci = words sadrzaj
        najduza = maximumBy (comparing length) sveReci
        duzina = length najduza
        izlaz = "Najduza rec: " ++ najduza ++ "\nDuzina: " ++ show duzina ++ "\n"
    writeFile "najduza.txt" izlaz
