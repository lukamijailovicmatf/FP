-- Napisati program koji ispisuje broj navedenih argumenata komandne linije, a zatim i same argumente i njihove redne brojeve.
-- Broj argumenata je 5.
-- 0: argumenti
-- 1: d1.txt
-- 2: 10
-- 3: 13.5
-- 4: d2.txt
        
module Main where

import System.Environment (getProgName, getArgs)
import Control.Monad (forM_)

main :: IO ()
main = do
    imePrograma <- getProgName
    argumenti <- getArgs
    let sviArgumenti = imePrograma : argumenti
    let brojArgumenata = length sviArgumenti
    putStrLn ("Broj argumenata je " ++ show brojArgumenata ++ ".")
    forM_ (zip [0..] sviArgumenti) (\(indeks, argument) -> do
                                                            putStrLn (show indeks ++ ": " ++ argument))
