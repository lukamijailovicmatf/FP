-- Ucitava se proizvoljan broj fajlova kao argumenti komandne linije i racuna se prosek brojeva u svakom fajlu.

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

izracunajProsek :: String -> Maybe Double
izracunajProsek sadrzaj = 
    let reci = words sadrzaj
        brojevi = mapMaybe readMaybe reci :: [Double]
    in if null brojevi then Nothing else Just (sum brojevi / fromIntegral (length brojevi))
    
obradiFajl :: FilePath -> IO ()
obradiFajl putanja = do
    sadrzaj <- readFile putanja
    case izracunajProsek sadrzaj of
        Just prosek -> putStrLn (putanja ++ ": " ++ show (prosek))
        Nothing -> putStrLn (putanja ++ ": Nema validnih brojeva ili je fajl prazan.")
        
main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti then putStrLn ("Nema argumenata komandne linije, potrebno ih je navesti") else mapM_ obradiFajl argumenti
