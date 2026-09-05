-- Imena fajlova se daju kao argumenti komandne linije i učitava se reč sa stdin-a. 
-- Prebrojati pojavljivanje te reči u fajlovima.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    trazenaRec <- getLine
    rezultati <- mapM (\putanja -> 
        do
          sadrzaj <- readFile putanja
          let broj = prebrojRec trazenaRec sadrzaj
          putStrLn (putanja ++ ": " ++ show broj)
          return broj
          ) argumenti
    putStrLn ("Ukupno: " ++ show (sum rezultati))
    
prebrojRec :: String -> String -> Int
prebrojRec trazenaRec sadrzaj = length (filter (\rec -> rec == trazenaRec) (words sadrzaj))
