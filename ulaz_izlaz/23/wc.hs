-- Argumenti komandne linije. Citanje fajla iz prvog argumenta. Ispis broja reci, linija i slicno, na osnovu drugog argumenta.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let putanja = argumenti !! 0
    let opcija = argumenti !! 1
    sadrzaj <- readFile putanja
    obradiSadrzaj opcija sadrzaj
    
obradiSadrzaj :: String -> String -> IO ()
obradiSadrzaj opcija sadrzaj = 
    case opcija of
        "-l" -> print (length (lines sadrzaj))
        "-w" -> print (length (words sadrzaj))
        "-c" -> print (length sadrzaj)
        "-a" -> putStrLn $ "Broj linija: " ++ show brojLinija ++ ", Broj reci: " ++ show brojReci ++ ", Broj karaktera: " ++ show brojKaraktera
        where brojLinija = length (lines sadrzaj)
              brojReci = length (words sadrzaj)
              brojKaraktera = length sadrzaj
