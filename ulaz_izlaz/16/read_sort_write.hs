-- Program uzima nekoliko argumenata (imena fajlova) i treba da otvori te fajlove i uzme sve redove i da ih sortira 
-- zajedno i ispise (cat + sort). Ukoliko nema argumenata treba da cita linije sa std ulaza.

module Main where

import System.Environment (getArgs)
import Data.List (sort)

main :: IO ()
main = do
    argumenti <- getArgs
    ulazniTekst <- if null argumenti then getContents else procitajSveFajlove argumenti
    let sveLinije = lines ulazniTekst
        sortiraneLinije = sort sveLinije
        konacniTekst = unlines sortiraneLinije
    putStr konacniTekst
    
procitajSveFajlove :: [FilePath] -> IO String
procitajSveFajlove putanje = do
    listaSadrzaja <- mapM readFile putanje
    return (concat listaSadrzaja)
