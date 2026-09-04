-- U fajlovima datim kao argument komandne linije ispisati najduzu rec u svakom redu (vise fajlova).

module Main where

import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = do
    argumenti <- getArgs
    if null argumenti then putStrLn ("Greska. Potrebno je navesti barem jedan argument komandne linije.") else mapM_ obradiFajl argumenti
    
obradiFajl :: FilePath -> IO ()
obradiFajl putanja = do
    sadrzaj <- readFile putanja
    let sveLinije = lines sadrzaj
    let najduzeReci = map najduzaRecULiniji sveLinije
    mapM_ putStrLn najduzeReci
    
najduzaRecULiniji :: String -> String
najduzaRecULiniji linija = case words linija of
                                [] -> ""
                                reci -> maximumBy (comparing length) reci
