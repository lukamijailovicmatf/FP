-- Definisati rekurzivni tip Stablo a koji moze biti Prazno ili sadrzi Cvor sa vrednoscu, levim stablom i desnim stablom. Napisati funkciju ubaci
-- za umetanje elementa u binarno stablo pretrage, funkciju velicina koja prebrojava ukupan broj cvorova u stablu, funkciju dubina koja racuna kolika
-- je ukupna dubina stabla i funkciju suma koja racuna zbir svih cvorova u stablu.

-- module Main where

data Stablo a = Prazno
              | Cvor a (Stablo a) (Stablo a)
              deriving (Show, Eq)
              
ubaci :: Ord a => a -> Stablo a -> Stablo a
ubaci x Prazno = Cvor x Prazno Prazno
ubaci x (Cvor v levo desno)
    | x < v = Cvor v (ubaci x levo) desno
    | x > v = Cvor v levo (ubaci x desno)
    | otherwise = Cvor v levo desno

velicina :: Stablo a -> Int
velicina Prazno = 0
velicina (Cvor _ levo desno) = 1 + velicina levo + velicina desno

dubina :: Stablo a -> Int
dubina Prazno = 0
dubina (Cvor _ levo desno) = 1 + max (dubina levo) (dubina desno)

suma :: Num a => Stablo a -> a
suma Prazno = 0
suma (Cvor x levo desno) = x + suma levo + suma desno

{-
main :: IO ()
main = do
    let s0 = Prazno
        s1 = ubaci 5 s0
        s2 = ubaci 3 s1
        s3 = ubaci 8 s2
        s4 = ubaci 1 s3
        s5 = ubaci 10 s4
    putStrLn ("Stablo: " ++ show s5)
    putStrLn ("Ukupno cvorova: " ++ show (velicina s5))
    putStrLn ("Dubina stabla: " ++ show (dubina s5))
    putStrLn ("Suma cvorova: " ++ show (suma s5))
-}
