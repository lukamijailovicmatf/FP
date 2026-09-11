-- Napisati program koji ispisuje sve opcije koje su navedene u komandnoj liniji. 
-- Opcije su karakteri argumenata komandne linije koji za koje važi da počinju karakterom -.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let filtrirani = filter (\argument -> head argument == '-') argumenti
    let opcije = concatMap tail filtrirani
    if null opcije
        then putStrLn ("Medju argumentima nema opcija.")
        else putStrLn ("Opcije su: " ++ unwords (map (\c -> [c]) opcije))
