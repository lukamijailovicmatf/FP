-- U fajlu rang_lista.txt nalaze se redovi sa imenom i prosekom. Sortirati studente tako da oni sa vecim prosekom
-- budu prvi. Ako dva studenta imaju isti prosek, sortirati ih abecedno po imenu. Sacuvati u rangirano.txt.

module Main where

import Data.List (sortBy)

data Student = Student { ime :: String,
                         prosek :: Double
                       }
                       
parsiraj :: String -> Student
parsiraj red = 
    case words red of
        [i, p] -> Student i (read p)
        _ -> Student "" 0.0
        
porediStudente :: Student -> Student -> Ordering
porediStudente s1 s2 = 
    case compare (prosek s2) (prosek s1) of
        EQ -> compare (ime s1) (ime s2)
        rezultat -> rezultat
        
formatiraj :: Student -> String
formatiraj s = ime s ++ " " ++ show (prosek s)
        
main :: IO ()
main = do
    sadrzaj <- readFile "rang_lista.txt"
    let studenti = map parsiraj (lines sadrzaj)
        sortirani = sortBy porediStudente studenti
        izlaz = unlines (map formatiraj sortirani)
    writeFile "rangirano.txt" izlaz
