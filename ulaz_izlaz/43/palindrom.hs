-- Napisati program koji iz datoteke razno.txt u datoteku palindromi.txt prepisuje sve palindrome. 
-- Reč je palindrom ako se isto čita sa leve i desne strane bez obzira na veličinu slova. 
-- Pretpostaviti da je maksimalna dužina reči 200 karaktera.

module Main where

import Data.Char (toLower)

main :: IO ()
main = do
    sadrzaj <- readFile "razno.txt"
    let sveReci = words sadrzaj
    let palindromi = filter jePalindrom sveReci
    writeFile "palindromi.txt" (unwords palindromi ++ "\n")
    
jePalindrom :: String -> Bool
jePalindrom rec = malaRec == reverse malaRec
    where malaRec = map toLower rec
