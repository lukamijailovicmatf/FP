-- U datoteci čije se ime zadaje sa standardnog ulaza nalazi se broj n (n ≤ 256), a zatim i n reči. 
-- Napisati program koji učitava reči iz datoteke u niz i iz niza uklanja sve duplikate i upisuje izmenjeni niz u datoteku bez_duplikata.txt 
-- Pretpostaviti da je maksimalna dužina naziva datoteke 20 karaktera, a maksimalna dužina jedne reči u datoteci 50 karaktera.

module Main where

import Data.List (nub)

main :: IO ()
main = do
    putStrLn ("Unesite ime datoteke:")
    imeDatoteke <- getLine
    sadrzaj <- readFile imeDatoteke
    let sviTokeni = words sadrzaj
    let n = read (head sviTokeni) :: Int
    let reci = take n (tail sviTokeni)
    let jedinstveneReci = ukloniDuplikate reci
    writeFile "bez_duplikata.txt" (unwords jedinstveneReci ++ "\n")
    
ukloniDuplikate :: [String] -> [String]
ukloniDuplikate reci = nub reci
