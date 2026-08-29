-- Ucitati fajl u kojem u redovima stoje zapisani podaci o studentima i ocenama tipa
-- Ana 9 8 10
-- Marko 7 8 9
-- Milena 9 9 9
-- Treba izracunati prosecnu ocenu za svakog studenta i ispisati sve studente i proseke u formatu “Ime: prosek”
-- Redosled ispisa treba da bude sortiran po proseku opadajuce, a ako je isti prosek onda se gleda po imenu rastuce.
-- Znaci za ovaj primer bi bilo 
-- Ana: 9.0
-- Milena: 9.0
-- Marko: 8.0

module Main where

import Data.List (sortBy)

data Student = Student { ime :: String,
                         prosek :: Double
                       } deriving (Show)
                       
parsirajRed :: String -> Student
parsirajRed red = 
    case words red of
        (imeStudenta : oceneTekst) -> let ocene = map read oceneTekst :: [Double]
                                          prosek = if null ocene then 0.0 else sum ocene / fromIntegral (length ocene)
                                      in Student imeStudenta prosek
        [] -> Student "" 0.0
        
porediStudente :: Student -> Student -> Ordering
porediStudente s1 s2 =
    case compare (prosek s2) (prosek s1) of
        EQ -> compare (ime s1) (ime s2)
        rezultat -> rezultat
        
formatiraj :: Student -> String
formatiraj s = ime s ++ ": " ++ show (prosek s)
        
main :: IO ()
main = do
    sadrzaj <- readFile "studenti.txt"
    let sviRedovi = lines sadrzaj
        studenti = map parsirajRed sviRedovi
        sortirani = sortBy porediStudente studenti
        izlaz = unlines (map formatiraj sortirani)
    putStr izlaz
