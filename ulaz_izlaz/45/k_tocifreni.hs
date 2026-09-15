-- U datoteci čije se ime zadaje kao prvi argument komandne linije nalazi se ceo pozitivan broj n, a zatim i n celih brojeva. 
-- Napisati program koji na standardni izlaz ispisuje koliko k-tocifrenih brojeva postoji u datoteci, pri čemu se pozitivan 
-- ceo broj k zadaje kao drugi argument komandne linije.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let imeDatoteke = argumenti !! 0
        k = read (argumenti !! 1) :: Int
    sadrzaj <- readFile imeDatoteke
    let sviTokeni = words sadrzaj
        n = read (head sviTokeni) :: Int
        brojevi = map read (take n (tail sviTokeni)) :: [Int]
    let kTocifreni = filter (\x -> brojCifara x == k) brojevi
        rezultat = length kTocifreni
    putStrLn ("Broj " ++ show k ++ "-cifrenih brojeva: " ++ show rezultat)
    
brojCifara :: Int -> Int
brojCifara x = length (show (abs x))
