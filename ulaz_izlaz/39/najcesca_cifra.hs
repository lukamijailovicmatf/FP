-- Napisati program koji u datoteci čije se ime unosi sa standardnog ulaza prebrojava koliko se puta pojavljuje svaka od cifara i na standardni
-- izlaz ispisuje cifru sa najvećim brojem pojavljivanja. Ukoliko ima više takvih cifara, ispisati sve. Ukoliko datoteka ne sadrži nijednu cifru, 
-- ispisati odgovarajuću poruku. Pretpostaviti da je maksimalna dužina naziva datoteke 20 karaktera. U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)
import Data.Char (isDigit)

main :: IO ()
main = do
    putStrLn ("Unesite ime datoteke:")
    imeDatoteke <- getLine
    postoji <- doesFileExist imeDatoteke
    case postoji of
        True -> do
            sadrzaj <- readFile imeDatoteke
            let sveCifre = filter isDigit sadrzaj
            if null sveCifre
                then putStrLn ("Datoteka ne sadrzi cifre.")
                else do
                    let brojevi = [(c, brojPojavljivanja c sveCifre) | c <- ['0'..'9']]
                    let maxBrojac = maximum [brojac | (_, brojac) <- brojevi]
                    let najcesce = [c | (c, brojac) <- brojevi, brojac == maxBrojac]
                    let ispisCifara = unwords (map (:[]) najcesce)
                    putStrLn ("Najcesce cifre: " ++ ispisCifara)
        False -> do
            putStrLn ("Greska: neuspesno otvaranje datoteke")
            
brojPojavljivanja :: Char -> String -> Int
brojPojavljivanja c tekst = length (filter (==c) tekst)
