-- Prvi red datoteke matrica.txt sadrži dva cela broja manja od 50 koji predstavljaju redom broj vrsta i broj kolona realne matrice a. 
-- Svaki sledeći red sadrži po jednu vrstu matrice. Napisati program koji pronalazi sve elemente matrice a koji su jednaki zbiru svih 
-- svojih susednih elemenata i štampa ih u obliku (broj vrste, broj kolone, vrednost elementa). Pretpostaviti da je sadržaj datoteke ispravan.
-- Elementi matrice a susedni elementu a[i][j] su svi elementi matrice čiji se indeksi, po apsolutnoj vrednosti, razlikuju najviše za jedan.

module Main where

main :: IO ()
main = do
    sadrzaj <- readFile "matrica.txt"
    let sviTokeni = words sadrzaj
        m = read (sviTokeni !! 0) :: Int
        n = read (sviTokeni !! 1) :: Int
        sviBrojevi = map read (drop 2 sviTokeni) :: [Int]
        mat = napraviMatricu n sviBrojevi
    let rezultati = [(i,j,val) | i <- [0..m-1], j <- [0..n-1], let val = (mat !! i) !! j, val == zbirSuseda mat m n i j]
    mapM_ (\(i,j,val) -> putStrLn ("(" ++ show i ++ ", " ++ show j ++ ", " ++ show val ++ ")")) rezultati
    
napraviMatricu :: Int -> [Int] -> [[Int]]
napraviMatricu _ [] = []
napraviMatricu n xs = take n xs : napraviMatricu n (drop n xs)

zbirSuseda :: [[Int]] -> Int -> Int -> Int -> Int -> Int
zbirSuseda mat m n i j = sum [(mat !! r) !! c | r <- [max 0 (i-1) .. min (m-1) (i+1)],
                                                c <- [max 0 (j-1) .. min (n-1) (j+1)],
                                                (r,c) /= (i,j)
                             ]
