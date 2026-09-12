-- Napisati program koji u datoteci čije je ime dato kao argument komandne linije proverava da li su zagrade pravilno uparene.
-- U slučaju greške, ispisati odgovarajuću poruku.

module Main where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    case argumenti of
        [putanja] -> do
            postoji <- doesFileExist putanja
            case postoji of
                True -> do
                    sadrzaj <- readFile putanja
                    if daLiSuUparene sadrzaj
                        then putStrLn ("Zagrade jesu uparene.")
                        else putStrLn ("Zagrade nisu uparene.")
                False -> do
                    putStrLn ("Greska: neuspesno otvaranje datoteke.")
        _ -> putStrLn ("Greska: neispravni argumenti komandne linije.")
        
daLiSuUparene :: String -> Bool
daLiSuUparene tekst = proveri tekst 0
    where proveri _ brojac | brojac < 0 = False
          proveri "" brojac = brojac == 0
          proveri (x:xs) brojac
            | x == '(' = proveri xs (brojac + 1)
            | x == ')' = proveri xs (brojac - 1)
            | otherwise = proveri xs brojac
