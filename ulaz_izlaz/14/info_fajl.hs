-- Kao prvi argument komandne linije je data putanja do fajla, a kao drugi argument se zadaje neka od sledecih opcija:
-- -l (lines): prebrojava ukupan broj linija u fajlu.
-- -m (characters): prebrojava ukupan broj karaktera (simbola) u fajlu.
-- -w (words): prebrojava ukupan broj reci u fajlu.
-- -L (max line length): pronalazi duzinu najduze reci u fajlu.

module Main where

import System.Environment (getArgs)

obradiSadrzaj :: String -> String -> Either String Int
obradiSadrzaj opcija sadrzaj = 
    case opcija of
        "-l" -> Right (length (lines sadrzaj))
        "-m" -> Right (length sadrzaj)
        "-w" -> Right (length (words sadrzaj))
        "-L" -> Right (najduzaLinija sadrzaj)
        _ -> Left ("Nepoznata opcija: " ++ opcija ++ ". Dozvoljene opcije su -l, -m, -w i -L.")
        where
            najduzaLinija s = 
                let linije = lines s
                in if null linije then 0 else maximum (map length linije)
                
main :: IO ()
main = do
    argumenti <- getArgs
    case argumenti of
        [putanja, opcija] -> do
            sadrzaj <- readFile putanja
            case obradiSadrzaj opcija sadrzaj of
                Right rezultat -> putStrLn (show rezultat)
                Left greska -> putStrLn ("Greska: " ++ greska)
        _ -> putStrLn ("Greska. Program mora primiti tacno dva argumenta.")
