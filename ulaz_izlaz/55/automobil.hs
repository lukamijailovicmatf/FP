-- Definisati strukturu Automobil koja sadrži marku, model i cenu. 
-- Napisati program koji iz datoteke čije se ime zadaje sa standardnog ulaza
-- učitava broj automobila i podatke za svaki automobil i zatim:
-- (a) ispisuje prosečnu cenu po marki automobila
-- (b) za maksimalnu cenu koju je kupac spreman da plati, a koja se zadaje kao argument komandne linije, ispisuje automobile u tom cenovnom rangu
-- Pretpostaviti da se model i marka sastoje od jedne reči, da svaka od njih sadrži
-- najviše 30 karaktera i da se u datoteci nalaze podaci za najviše 100 automobila.

module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Data.List (nub)

data Automobil = Automobil { marka :: String,
                             model :: String,
                             cena :: Int
                           } deriving (Show)

parsirajAutomobile :: Int -> [String] -> [Automobil]
parsirajAutomobile 0 _ = []
parsirajAutomobile n (marka : model : cena : ostatak) = 
    Automobil marka model (read cena) : parsirajAutomobile (n - 1) ostatak
    
prosekMarke :: String -> [Automobil] -> Double
prosekMarke m automobili = 
    let cene = [cena a | a <- automobili, marka a == m]
    in fromIntegral (sum cene) / fromIntegral (length cene)

main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let maxCena = read (head argumenti) :: Int
            putStrLn ("Unesite naziv datoteke:")
            imeDatoteke <- getLine
            postoji <- doesFileExist imeDatoteke
            if not postoji
                then putStrLn ("Greska: neuspesno otvaranje ulazne datoteke.")
                else do
                    sadrzaj <- readFile imeDatoteke
                    let sviTokeni = words sadrzaj
                    if null sviTokeni
                        then putStrLn ("Greska: prazna datoteka.")
                        else do
                            let n = read (head sviTokeni) :: Int
                                automobili = parsirajAutomobile n (tail sviTokeni)
                                jedinstveneMarke = nub (map marka automobili)
                            putStrLn ("Informacije o prosecnoj ceni po markama:")
                            mapM_ (\m -> printf "%s %.2f\n" m (prosekMarke m automobili)) jedinstveneMarke
                            putStrLn ("Kola u Vasem cenovnom rangu:")
                            let uRangu = filter (\a -> cena a <= maxCena) automobili
                            mapM_ (\a -> putStrLn (marka a ++ " " ++ model a ++ " " ++ show (cena a))) uRangu
