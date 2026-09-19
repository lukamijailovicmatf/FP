-- U fajlu ispit.txt nalaze se podaci o studentima u formatu "IME POENI". Procitati fajl, izdvojiti samo studente koji
-- imaju 51 ili vise poena i njihova imena upisati u polozili.txt.

module Main where

data Student = Student { ime :: String,
                         poeni :: Int
                       }
                       
parsiraj :: String -> Student
parsiraj red =
    case words red of
        [i, p] -> Student i (read p)
        _ -> Student "Greska" 0

main :: IO ()
main = do
    sadrzaj <- readFile "ispit.txt"
    let studenti = map parsiraj (lines sadrzaj)
        polozili = filter (\s -> poeni s >= 51) studenti
        imena = map ime polozili
        izlaz = unlines imena
    writeFile "polozili.txt" izlaz
