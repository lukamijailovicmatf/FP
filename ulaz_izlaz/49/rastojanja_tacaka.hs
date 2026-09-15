-- U datoteci tacke.txt se nalazi broj tačaka, a zatim u posebnim redovima za svaku tačku njene x i y koordinate. 
-- Napisati program koji u datoteku rastojanja.txt upisuje rastojanje svake od učitanih tačaka od koordinatnog početka, 
-- a na standardni izlaz koordinate tačke koja je od njega najudaljenija. Ukoliko ima više takvih tačaka, ispisati koordinate prve. 
-- Koristiti strukturu Tacka sa poljima x i y, kao i funkciju kojom se računa rastojanje tačke
-- od koordinatnog početka. Pretpostaviti da je maksimalan broj tačaka u datoteci 50.

module Main where

import Text.Printf (printf)

data Tacka = Tacka { xKord :: Double,
                     yKord :: Double
                   } deriving (Show)
                   
paroviUTacke :: [Double] -> [Tacka]
paroviUTacke [] = []
paroviUTacke (x : y : ostatak) = Tacka x y : paroviUTacke ostatak
paroviUTacke _ = []

rastojanje :: Tacka -> Double
rastojanje (Tacka x y) = sqrt (x * x + y * y)

prikazi :: Double -> String
prikazi val
    | val == fromIntegral (floor val) = show (floor val :: Int)
    | otherwise = printf "%.2f" val

main :: IO ()
main = do
    sadrzaj <- readFile "tacke.txt"
    let sviTokeni = words sadrzaj
    if null sviTokeni
        then putStrLn ("Greska: neispravan broj tacaka.")
        else do
            let n = read (head sviTokeni) :: Int
            if n <= 0 || n > 50
                then putStrLn ("Greska: neispravan broj tacaka.")
                else do
                    let sviBrojevi = map read (tail sviTokeni) :: [Double]
                        tacke = take n (paroviUTacke sviBrojevi)
                        svaRastojanja = map rastojanje tacke
                    let izlazRastojanja = unlines [printf "%.2f" r | r <- svaRastojanja]
                    writeFile "rastojanja.txt" izlazRastojanja
                    let najudaljenija = foldl1 (\t1 t2 -> if rastojanje t1 > rastojanje t2 then t1 else t2) tacke
                    putStrLn ("Najudaljenija tacka: (" ++ prikazi (xKord najudaljenija) ++ ", " ++ prikazi (yKord najudaljenija) ++ ")")
