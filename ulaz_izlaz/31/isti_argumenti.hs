-- Napisati program koji proverava da li se među zadatim argumentima komandne linije nalaze barem dva ista. 
-- Napomena: Uzeti u obzir i naziv programa koji se pokreće.

module Main where

import System.Environment (getProgName, getArgs)
import Data.List (nub)

main :: IO ()
main = do
    imePrograma <- getProgName
    argumenti <- getArgs
    let sviArgumenti = imePrograma : argumenti
    if length sviArgumenti /= length (nub sviArgumenti) 
        then putStrLn ("Medju argumentima ima istih.") 
        else putStrLn ("Medju argumentima nema istih.")
