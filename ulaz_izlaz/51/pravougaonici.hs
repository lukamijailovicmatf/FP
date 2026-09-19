-- Definisati tip Pravougaonik koja sadrži dužine stranica i ime pravougaonika. 
-- Napisati program koji iz datoteke čije ime se zadaje kao argument komandne linije 
-- učitava podatke o pravougaonicima (nije poznato koliko), a zatim ispisuje imena onih 
-- pravougaonika koji su kvadrati i vrednost najveće površine među pravougaonicima koji nisu kvadrati. 
-- Pretpostaviti da je maksimalan broj pravougaonika 200, a maksimalna dužina imena pravougaonika 4.

module Main where

import System.Environment (getArgs)

data Pravougaonik = Pravougaonik { a :: Int,
                                   b :: Int,
                                   ime :: String
                                 } deriving (Show)
                                 
povrsina :: Pravougaonik -> Int
povrsina p = a p * b p

jeKvadrat :: Pravougaonik -> Bool
jeKvadrat p = a p == b p

grupisiUTrojke :: [String] -> [Pravougaonik]
grupisiUTrojke (strA : strB : imeP : ostatak) = Pravougaonik (read strA) (read strB) imeP : grupisiUTrojke ostatak
grupisiUTrojke _ = []

main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti
        then putStrLn ("Greska: nije naveden nijedan argument komandne linije.")
        else do
            let imeDatoteke = head argumenti
            sadrzaj <- readFile imeDatoteke
            let sviTokeni = words sadrzaj
                pravougaonici = grupisiUTrojke sviTokeni
            let kvadrati = filter jeKvadrat pravougaonici
                neKvadrati = filter (not . jeKvadrat) pravougaonici
            let imenaKvadrata = unwords (map ime kvadrati)
            let maxPovrsina = if null neKvadrati then "" else show (maximum (map povrsina neKvadrati))
            let izlaz = unwords (filter (not . null) [imenaKvadrata, maxPovrsina])
            putStrLn izlaz
