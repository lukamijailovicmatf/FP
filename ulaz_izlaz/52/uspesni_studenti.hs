-- U datoteci studenti.txt se nalaze podaci o studentima. Za svakog studenta je dato korisničko ime 
-- na Alas serveru i poslednjih pet ocena koje je dobio. Napisati program koji pronalazi studenta 
-- koji je ostvario najbolji uspeh i ispisuje njegove podatke. Ukoliko ima više takvih studenata, 
-- ispisati informacije o svima. Pretpostaviti da je maksimalni broj studenta 100.

module Main where

import Text.Printf (printf)

data Student = Student { korisnickoIme :: String,
                         ocene :: [Int]
                       } deriving (Show)
                       
prosek :: Student -> Double
prosek s = fromIntegral (sum (ocene s)) / 5.0

paroviStudenata :: [String] -> [Student]
paroviStudenata (ime : o1 : o2 : o3 : o4 : o5 : ostatak) = 
    Student ime [read o1, read o2, read o3, read o4, read o5] : paroviStudenata ostatak
paroviStudenata _ = []

main :: IO ()
main = do
    sadrzaj <- readFile "studenti.txt"
    let sviTokeni = words sadrzaj
        studenti = paroviStudenata sviTokeni
    if null studenti
        then putStrLn ("Nema studenata u datoteci.")
        else do
            let studentiSaProsecima = [(s, prosek s) | s <- studenti]
            let maxProsek = maximum [p | (_, p) <- studentiSaProsecima]
            let najboljiStudenti = [s | (s, p) <- studentiSaProsecima, p == maxProsek]
            putStrLn ("Studenti sa najvecim prosekom:")
            mapM_ ispisiStudenta najboljiStudenti
            
ispisiStudenta :: Student -> IO ()
ispisiStudenta s = do
    putStrLn ("Korisnicko ime: " ++ korisnickoIme s)
    let p = prosek s
    putStrLn ("Prosek ocena: " ++ printf "%.2f" p)
