-- Iz fajla koji se prosledjuje kao argument komandne linije cita se niska "ababbabababbbbaba". 
-- Izracunati koliko najmanje slova treba da se promeni da ne postoje 3 uzastopna ista znaka.

-- verzija koda napisana za fajl koji moze da sadrzi i vise niski

module Main where

import System.Environment (getArgs)
import Data.List (group)

main :: IO ()
main = do
    argumenti <- getArgs
    let putanja = head argumenti
    sadrzaj <- readFile putanja
    let sveLinije = lines sadrzaj
    let rezultati = map minimalnoIzmena sveLinije
    mapM_ print rezultati
    
minimalnoIzmena :: String -> Int
minimalnoIzmena niska = sum [(length blok) `div` 3 | blok <- group niska]
