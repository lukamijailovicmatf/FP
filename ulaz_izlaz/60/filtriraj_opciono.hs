-- Napisati program koji linije koje se učitavaju sa standardnog ulaza sve do kraja ulaza prepisuje u datoteku izlaz.txt i to, ako je prilikom
-- pokretanja zadata opcija -v ili -V samo one linije koje počinju velikim slovom, ako je zadata opcija -m ili -M samo one linije koje počinju 
-- malim slovom, a ako je opcija izostavljena sve linije. Pretpostaviti da je maksimalna dužina linije 80 karaktera.

module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Data.Char (isUpper, isLower)

proveriLiniju :: String -> String -> Bool
proveriLiniju opcija linija
    | null linija = False
    | opcija `elem` ["-v", "-V"] = isUpper (head linija)
    | opcija `elem` ["-m", "-M"] = isLower (head linija)
    | otherwise = True

main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti
        then do
            putStrLn ("Unesite recenice:")
            hFlush stdout
            sadrzaj <- getContents
            writeFile "izlaz.txt" sadrzaj
        else do
            let opcija = head argumenti
            if opcija `notElem` ["-v", "-V", "-m", "-M"]
                then putStrLn ("Greska: neispravna opcija.")
                else do
                    putStrLn ("Unesite recenice:")
                    hFlush stdout
                    sadrzaj <- getContents
                    let sveLinije = lines sadrzaj
                    let filtrirane = filter (\linija -> proveriLiniju opcija linija) sveLinije
                    writeFile "izlaz.txt" (unlines filtrirane)
