-- Napisati program koji cita fajl log.txt i u novi fajl log_greske.txt upisuje samo one redove koji
-- u sebi sadrze rec "GRESKA".

module Main where

import Data.List (isInfixOf)

main :: IO ()
main = do
    sadrzaj <- readFile "log.txt"
    let sviRedovi = lines sadrzaj
        filtrirani = filter (\red -> "GRESKA" `isInfixOf` red) sviRedovi
        izlaz = unlines filtrirani
    writeFile "log_greske.txt" izlaz
