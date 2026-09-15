-- Definisati strukturu kojom se opisuje trodimenzioni vektor sa celobrojnim koordinatama x, y i z. 
-- U datoteci vektori.txt nalazi se nepoznati broj vektora. Napisati program koji učitava vektore iz ove datoteke i na
-- standardni izlaz ispisuje koordinate vektora sa najvećom dužinom. Ukoliko ima više takvih vektora, ispisati koordinate prvog. 
-- Dužina vektora se izračunava po formuli: |v| = sqrt (x^2 + y^2 + z^2).

module Main where

data Vektor = Vektor { xKord :: Int,
                       yKord :: Int,
                       zKord :: Int
                     } deriving (Show)
                     
trojkeUVektore :: [Int] -> [Vektor]
trojkeUVektore (x : y : z : ostatak) = Vektor x y z : trojkeUVektore ostatak
trojkeUVektore _ = []

duzinaKvadrat :: Vektor -> Int
duzinaKvadrat (Vektor x y z) = x * x + y * y + z * z

main :: IO ()
main = do
    sadrzaj <- readFile "vektori.txt"
    let sviBrojevi = map read (words sadrzaj) :: [Int]
        vektori = trojkeUVektore sviBrojevi
        najduzi = foldl1 (\v1 v2 -> if duzinaKvadrat v1 > duzinaKvadrat v2 then v1 else v2) vektori
    putStrLn (show (xKord najduzi) ++ " " ++ show (yKord najduzi) ++ " " ++ show (zKord najduzi))
