-- Prima se argument, za dati argument komande linije ucitati fajl i parsirati izraz, npr. "4 3 2 * +" = (3 + 2) *4  = 20, mogu biti i visecifreni brojevi.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    case argumenti of
        [putanjaDoFajla] -> do
            sadrzaj <- readFile putanjaDoFajla
            let tokeni = words sadrzaj
                rezultat = izracunajIzraz tokeni
            putStrLn ("Rezultat izraza je " ++ show rezultat)
        _ -> putStrLn ("Greska. Morate da prosledite tacno jedan argument.")
        
izracunajIzraz :: [String] -> Int
izracunajIzraz tokeni = head (foldl obradiToken [] tokeni)
    where obradiToken (x : y : ostatak) "+" = (y + x) : ostatak
          obradiToken (x : y : ostatak) "-" = (y - x) : ostatak
          obradiToken (x : y : ostatak) "*" = (y * x) : ostatak
          obradiToken (x : y : ostatak) "/" = (y `div` x) : ostatak
          obradiToken stek brojTekst = (read brojTekst :: Int) : stek
