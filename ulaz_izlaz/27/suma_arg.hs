-- Napisati program koji ispisuje zbir celorojnih argumenata komandne linije?

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    argumenti <- getArgs
    let celiBrojevi = mapMaybe readMaybe argumenti :: [Int]
    let sumaCelihBrojeva = sum celiBrojevi
    putStrLn ("Zbir celobrojnih argumenata: " ++ show sumaCelihBrojeva)
