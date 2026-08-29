-- Ucitati fajl poruka.txt i sve pojave reci "RUZNA" zameniti sa "*****". Rezultat sacuvati u cenzurisano.txt.

module Main where

zameniRec :: String -> String
zameniRec rec
    | rec == "RUZNA" = "*****"
    | otherwise = rec
    
zameniRecURedu :: String -> String
zameniRecURedu red = unwords (map zameniRec (words red))

main :: IO ()
main = do
    sadrzaj <- readFile "poruka.txt"
    let sviRedovi = lines sadrzaj
        obradjeni = map zameniRecURedu sviRedovi
        izlaz = unlines obradjeni
    writeFile "cenzurisano.txt" izlaz
