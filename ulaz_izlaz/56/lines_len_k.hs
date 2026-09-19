--  Kao argumenti komandne linije zadaju se ime datoteke i ceo broj k. Napisati program koji na standardni izlaz ispisuje sve linije zadate
-- datoteke čija je dužina veća od k. Pretpostaviti da je maksimalna dužina linije 80 karaktera.

module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
    argumenti <- getArgs
    if length argumenti /= 2
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let imeDatoteke = argumenti !! 0
                k = read (argumenti !! 1) :: Int
            postoji <- doesFileExist imeDatoteke
            if not postoji
                then putStrLn ("Greska: neuspesno otvaranje ulazne datoteke.")
                else do
                    sadrzaj <- readFile imeDatoteke
                    let sveLinije = lines sadrzaj
                    let linijeDuzeOdK = filter (\linija -> length linija > k) sveLinije
                    mapM_ putStrLn linijeDuzeOdK
