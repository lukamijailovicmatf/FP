-- Prvi red datoteke ulaz.txt sadrži dva cela broja između 2 i 50 koji predstavljaju redom broj vrsta i broj kolona realne matrice a. 
-- Svaki sledeći red sadrži po jednu vrstu matrice. Napisati program koji nalazi i štampa sve četvorke oblika (a(i,j), a(i+1,j), a(i,j+1), a(i+1,j+1))
-- u kojima su svi elementi međusobno različiti.

module Main where

import Data.List (nub)

napraviMatricu :: Int -> [Int] -> [[Int]]
napraviMatricu _ [] = []
napraviMatricu n xs = take n xs : napraviMatricu n (drop n xs)

sviRazliciti :: [Int] -> Bool
sviRazliciti xs = length (nub xs) == 4

main :: IO ()
main = do
    sadrzaj <- readFile "ulaz.txt"
    let sviTokeni = words sadrzaj
        m = read (sviTokeni !! 0) :: Int
        n = read (sviTokeni !! 1) :: Int
    if m < 2 || m > 50 || n < 2 || n > 50
        then putStrLn ("Greska: neispravna dimenzija.")
        else do
            let sviBrojevi = map read (drop 2 sviTokeni) :: [Int]
                mat = napraviMatricu n sviBrojevi
            let cetvorke = [(x1, x2, x3, x4) |
                             i <- [0 .. m - 2],
                             j <- [0 .. n - 2],
                             let x1 = (mat !! i) !! j,
                             let x2 = (mat !! (i + 1)) !! j,
                             let x3 = (mat !! i) !! (j + 1),
                             let x4 = (mat !! (i + 1)) !! (j + 1),
                             sviRazliciti [x1, x2, x3, x4]
                           ]
            mapM_ (\(x1, x2, x3, x4) ->
                putStrLn ("(" ++ show x1 ++ ", " ++ show x2 ++ ", " ++ show x3 ++ ", " ++ show x4 ++ ")")) cetvorke
