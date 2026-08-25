-- U fajlu brojevi.txt nalaze se brojevi razdvojeni razmacima ili novim redovima. Procitati fajl, izracunati zbir i prosek svih
-- brojeva i ispisati ih na standardni izlaz.

module Main where

main :: IO ()
main = do
    sadrzaj <- readFile "brojevi.txt"
    let reci = words sadrzaj
        brojevi = map read reci :: [Double]
        zbir = sum brojevi
        prosek = if null brojevi then 0.0 else zbir / fromIntegral (length brojevi)
    putStrLn("Zbir: " ++ show zbir)
    putStrLn("Prosek: " ++ show prosek)
