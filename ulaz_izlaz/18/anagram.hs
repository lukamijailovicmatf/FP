-- 	Kao prvi argument komandne linije se dobije ime fajla, kao drugi argument dobijamo rec.
-- Treba ispisati sve redne brojeve linija u kojim se nalaze anagrami reci koja je proslednjena kao drugi argument.
-- Ne pravi se razlika izmedju velikih i malih slova
-- npr. za rec cat i tekst:
-- 	neki tekst Cat
--	koji se ne tac secam
--	kako ide bas najbolje
--	ali treba da se nadje act
--	da bi mogao zad da se uradi
-- treba vratiti redne brojeve linija, svaki broj u novom redu, rezultat:
-- 1
-- 2
-- 4

module Main where

import Data.List (sort, tails)
import Data.Char (toLower)
import System.Environment (getArgs)

main :: IO ()
main = do
    [putanja, rec] <- getArgs
    sadrzaj <- readFile putanja
    let meta = sort (map toLower rec)
        n = length rec
        imaAnagram linija = any (\deo -> length deo == n && sort (map toLower deo) == meta) (map (\sufiks -> take n sufiks) (tails linija))
        rezultat = [broj | (broj, linija) <- zip [1..] (lines sadrzaj), imaAnagram linija]
    mapM_ print rezultat
