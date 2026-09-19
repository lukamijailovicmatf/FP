-- Napisati program koji poredi dve datoteke i ispisuje redni broj linija u kojima se datoteke razlikuju. 
-- Imena datoteka se zadaju kao argumenti komandne linije. Pretpostaviti da je maksimalna dužina linije 200 karaktera. 
-- Linije brojati počevši od 1.

module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)

porediLinije :: Int -> [String] -> [String] -> [Int]
porediLinije _ [] [] = []
porediLinije indeksLinije (x:xs) [] = indeksLinije : porediLinije (indeksLinije + 1) xs []
porediLinije indeksLinije [] (y:ys) = indeksLinije : porediLinije (indeksLinije + 1) [] ys
porediLinije indeksLinije (x:xs) (y:ys)
    | x /= y = indeksLinije : porediLinije (indeksLinije + 1) xs ys
    | otherwise = porediLinije (indeksLinije + 1) xs ys

main :: IO ()
main = do
    argumenti <- getArgs
    if length argumenti /= 2
        then putStrLn ("Greska: neispravan poziv.")
        else do
            let file1 = argumenti !! 0
                file2 = argumenti !! 1
            postoji1 <- doesFileExist file1
            postoji2 <- doesFileExist file2
            if not postoji1 || not postoji2
                then putStrLn ("Greska: datoteka ne postoji.")
                else do
                    sadrzaj1 <- readFile file1
                    sadrzaj2 <- readFile file2
                    let linije1 = lines sadrzaj1
                        linije2 = lines sadrzaj2
                        razlike = porediLinije 1 linije1 linije2
                    putStrLn (unwords (map show razlike))
