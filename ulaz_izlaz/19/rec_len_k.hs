-- Kao argument komandne linije zadaje se putanja do fajla i broj k 
-- treba da ispises linije u kojima se nalazi bar jedna rec koja je duzina vece od k.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    case argumenti of
        [putanjaDoFajla, kStr] -> do
            let k = read kStr :: Int
            sadrzaj <- readFile putanjaDoFajla
            let sveLinije = lines sadrzaj
            let filtriraneLinije = filter (\linija -> recDuzaOdK k linija) sveLinije
            mapM_ putStrLn filtriraneLinije
        _ -> putStrLn ("Greska. Treba proslediti tacno dva argumenta.")
        
recDuzaOdK :: Int -> String -> Bool
recDuzaOdK k linija = any (\rec -> length rec > k) (words linija)
