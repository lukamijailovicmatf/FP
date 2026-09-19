-- Ucitati fajl kod.txt i izbaciti sve prazne redove kao i redove koji pocinju simbolom # (komentare).
-- Ocisceni sadrzaj sacuvati u cist_kod.txt.

module Main where

ispravanRed :: String -> Bool
ispravanRed red = 
    let tekst = dropWhile (== ' ') red
    in not (null tekst) && head tekst /= '#'
    
main :: IO ()
main = do
    sadrzaj <- readFile "kod.txt"
    let sviRedovi = lines sadrzaj
        ispravniRedovi = filter ispravanRed sviRedovi
        izlaz = unlines ispravniRedovi
    writeFile "cist_kod.txt" izlaz
