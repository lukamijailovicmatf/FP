-- Definisati strukturu Student koja sadrži puno ime studenta, niz njegovih ocena, broj ocena i prosečnu ocenu. 
-- U datoteci čije se ime zadaje kao argument komandne linije se nalaze podaci o studentima. Za svakog studenta dato je ime, 
-- prezime i niz ocena koji se završava nulom. Svi podaci su razdvojeni razmacima. Napisati program koji učitava podatke o 
-- studentima i na standardni izlaz ispisuje podatke za studenta sa najvećim prosekom (prosek ispisati na 2 decimale). 
-- Ukoliko ima više takvih studenata, ispisati informacije o prvom studentu. Pretpostaviti da je maksimalni broj ocena 10 
-- i maksimalna dužina punog imena 100 karaktera.

-- primer linije: Marko Markovic 5 6 7 8 9 0

module Main where

import System.Environment (getArgs)
import Text.Printf (printf)

data Student = Student { punoIme :: String,
                         ocene :: [Int],
                         brojOcena :: Int,
                         prosecnaOcena :: Double
                       } deriving (Show)
                       
parsirajStudente :: [String] -> [Student]
parsirajStudente [] = []
parsirajStudente (ime : prezime : ostatak) = 
    let (oceneTokeni, saNulom) = span (/= "0") ostatak
        listaOcena = map read oceneTokeni :: [Int]
        br = length listaOcena
        prosek = if br == 0 then 0.0 else fromIntegral (sum (listaOcena)) / fromIntegral br
        trenutniStudent = Student (ime ++ " " ++ prezime) listaOcena br prosek
        sledeciTokeni = drop 1 saNulom
    in trenutniStudent : parsirajStudente sledeciTokeni
parsirajStudente _ = []

main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let imeDatoteke = head argumenti
            sadrzaj <- readFile imeDatoteke
            let sviTokeni = words sadrzaj
                studenti = parsirajStudente sviTokeni
            if null studenti
                then putStrLn ("Nema podataka o studentima.")
                else do
                    let najbolji = foldl1 (\s1 s2 -> if prosecnaOcena s1 > prosecnaOcena s2 then s1 else s2) studenti
                    let ispisOcena = unwords (map show (ocene najbolji))
                        ispisProseka = printf "%.2f" (prosecnaOcena najbolji)
                    putStrLn (punoIme najbolji ++ " " ++ ispisOcena ++ " " ++ ispisProseka)
