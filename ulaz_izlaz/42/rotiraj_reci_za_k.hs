-- Sa standarnog ulaza se učitava ime datoteke i nenegativan ceo broj k. Napisati program koji učitava reči iz datoteke 
-- (reč je niz karaktera između blanko simbola) i svaku pročitanu reč rotira za k mesta u levo i tako dobijenu reč upisuje u datoteku 
-- čije je ime rotirano.txt. Pretpostaviti da je maksimalna dužina naziva datoteke 20 karaktera, da datoteka sadrži samo slova
-- i beline i da je maksimalna dužina jedne reči u datoteci 100 karaktera. U slučaju greške, ispisati odgovarajuću poruku.

module Main where

main :: IO ()
main = do
    putStrLn ("Unesite ime datoteke:")
    imeDatoteke <- getLine
    putStrLn ("Unesite broj k:")
    kStr <- getLine
    sadrzaj <- readFile imeDatoteke
    let k = read kStr :: Int
    let rotirane = map (rotiraj k) (words sadrzaj)
    writeFile "rotirano.txt" (unwords rotirane ++ "\n")
    
rotiraj :: Int -> String -> String
rotiraj k rec = drop k' rec ++ take k' rec
    where k' = k `mod` length rec
