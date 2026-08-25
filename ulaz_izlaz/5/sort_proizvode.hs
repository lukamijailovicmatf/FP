-- Fajl proizvodi.txt sadrzi nazive i cene proizvoda u formatu "NAZIV CENA". Procitati fajl, izdvojiti samo proizvode jeftinije
-- od 1000 dinara, sortirati ih po ceni rastuce i rezultat zapisati u jeftini.txt.

module Main where

import Data.List (sortBy)
import Data.Ord (comparing)

data Proizvod = Proizvod { naziv :: String,
                           cena :: Double
                         } deriving (Show)
                         
parsirajProizvod :: String -> Proizvod
parsirajProizvod red =
    case words red of
        [n, c] -> Proizvod n (read c)
        _ -> Proizvod "Greska" 0.0
        
formatirajProizvod :: Proizvod -> String
formatirajProizvod p = naziv p ++ " - " ++ show(cena p) ++ "rsd"

main :: IO ()
main = do
    sadrzaj <- readFile "proizvodi.txt"
    let proizvodi = map parsirajProizvod (lines sadrzaj)
        jeftini = filter (\p -> cena p < 1000) proizvodi
        sortirani = sortBy (comparing cena) jeftini
        rezultat = unlines (map formatirajProizvod sortirani)
    writeFile "jeftini.txt" rezultat
    
