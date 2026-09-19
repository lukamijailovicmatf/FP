-- Napisati program koji iz fajla ulaz.txt ucitava sadrzaj, prebrojava koliko ukupno ima redova i reci, a zatim
-- taj rezultat upisuje u fajl izvestaj.txt.

module Main where

main :: IO ()
main = do
    sadrzaj <- readFile "ulaz.txt"
    let brojRedova = length (lines sadrzaj)
        brojReci = length (words sadrzaj)
        izlaz = "Broj redova: " ++ show brojRedova ++ "\nBroj reci: " ++ show brojReci ++ "\n"
    writeFile "izvestaj.txt" izlaz
