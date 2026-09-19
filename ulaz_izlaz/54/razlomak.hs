-- Imena ulazne i izlazne datoteke se redom navode kao argumenti komandne linije. 
-- U ulaznoj datoteci se nalaze podaci o razlomcima: u prvom redu se nalazi broj razlomaka, 
-- a u svakom sledećem redu brojilac i imenilac po jednog razlomka. Definisati strukturu koja 
-- opisuje razlomak i napisati program koji učitava niz razlomaka iz datoteke, a potom:
-- (a) ukoliko je prilikom pokretanja programa navedena opcija x, upisati u izlaznu datoteku recipročni razlomak za svaki razlomak iz niza
-- (b) ukoliko je prilikom pokretanja programa navedena opcija y, upisati u izlaznu datoteku realnu vrednost recipročnog razlomka svakog razlomka iz niza
-- Pretpostaviti da se u ulaznoj datoteci nalazi najviše 100 razlomaka.

module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import System.Directory (doesFileExist)

data Razlomak = Razlomak { brojilac :: Int,
                           imenilac :: Int
                         } deriving (Show)
                         
reciprocni :: Razlomak -> Razlomak
reciprocni (Razlomak b i)
    | b < 0 = Razlomak (-i) (-b)
    | otherwise = Razlomak i b
    
obradiOpciju :: String -> Razlomak -> String
obradiOpciju "-x" r = 
    let (Razlomak b i) = reciprocni r
    in show b ++ "/" ++ show i
obradiOpciju "-y" r = 
    let (Razlomak b i) = reciprocni r
        realnaVrednost = fromIntegral b / fromIntegral i :: Double
    in printf "%.6f" realnaVrednost
obradiOpciju _ _ = ""

parsirajRazlomke :: Int -> [String] -> [Razlomak]
parsirajRazlomke 0 _ = []
parsirajRazlomke n (b : i : ostatak) = 
    Razlomak (read b) (read i) : parsirajRazlomke (n - 1) ostatak
parsirajRazlomke _ _ = []

main :: IO ()
main = do
    argumenti <- getArgs
    if length argumenti /= 3
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let ulaznaDatoteka = argumenti !! 0
                izlaznaDatoteka = argumenti !! 1
                opcija = argumenti !! 2
            if opcija /= "-x" && opcija /= "-y"
                then putStrLn ("Greska: neispravan poziv.")
                else do
                    postoji <- doesFileExist ulaznaDatoteka
                    if not postoji
                        then putStrLn ("Greska: neuspesno otvaranje ulazne datoteke.")
                        else do
                            sadrzaj <- readFile ulaznaDatoteka
                            let sviTokeni = words sadrzaj
                            if null sviTokeni
                                then putStrLn ("Greska: prazna datoteka.")
                                else do
                                    let n = read (head sviTokeni) :: Int
                                        razlomci = parsirajRazlomke n (tail sviTokeni)
                                        izlaz = unlines (map (obradiOpciju opcija) razlomci)
                                    writeFile izlaznaDatoteka izlaz
