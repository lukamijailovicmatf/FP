-- Ucitati sadrzaj fajla reci.txt i ispisati reci koje se najveci broj puta ponavljaju u fajlu. Sortirati reci tako da se prikazu one koje se 
-- najcesce ponavljaju, a ako se neke isti broj puta ponavljaju sortirati ih leksikografski. Ispisati ih u formatu "rec: broj ponavljanja".

module Main where

import Data.List (sort, sortBy, group)

data RecFrekvencija = RecFrekvencija { rec :: String,
                                       broj :: Int
                                     }
                                     
porediReci :: RecFrekvencija -> RecFrekvencija -> Ordering
porediReci r1 r2 = 
    case compare (broj r2) (broj r1) of
        EQ -> compare (rec r1) (rec r2)
        rezultat -> rezultat

izracunajFrekvencije :: [String] -> [RecFrekvencija]
izracunajFrekvencije sveReci = 
    let grupisane = group (sort sveReci)
    in map (\g -> RecFrekvencija (head g) (length g)) grupisane
    
formatiraj :: RecFrekvencija -> String
formatiraj r = rec r ++ ": " ++ show (broj r)

main :: IO ()
main = do
    sadrzaj <- readFile "reci.txt"
    let sveReci = words sadrzaj
        frekvencije = izracunajFrekvencije sveReci
        sortirane = sortBy porediReci frekvencije
        izlaz = unlines (map formatiraj sortirane)
    putStr izlaz
