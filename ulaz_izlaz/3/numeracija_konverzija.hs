-- Napisati program koji cita fajl tekst.txt, svako slovo svakog reda pretvoriti u veliko slovo
-- i dodati redni broj ispred svakog reda (npr. 1. TEKST), pa sacuvati u obradjeno.txt.

module Main where

import Data.Char (toUpper)

main :: IO ()
main = do
    sadrzaj <- readFile "tekst.txt"
    let sviRedovi = lines sadrzaj
        numerisani = zip [1..] sviRedovi
        obradjeni = map (\(rbr, recenica) -> show rbr ++ ". " ++ map (\rec -> toUpper rec) recenica) numerisani
        finalniTekst = unlines obradjeni
    writeFile "obradjeno.txt" finalniTekst 
