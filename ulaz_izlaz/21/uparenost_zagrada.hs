-- Kao argument komandne linije dobija se putanja do fajla. Potrebno je ocitati putanju, zatim procitati sadrzaj fajla i ispisati
-- redne brojeve linija u kojima su zagrade ispravno uparene. Posmatrati samo karaktere (), [], {} kao validne karaktere za uparivanje.
-- Resenje tj. redne brojeve ispisati na standardni izlaz.

-- Primer fajla ulaz.txt:
-- let lista = [1, 2, 3, 4, 5]
-- (]
-- {([])}
-- {1,2,3,()}[]

-- Izlaz:
-- 1
-- 3
-- 4

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    argumenti <- getArgs
    let putanja = head argumenti
    sadrzaj <- readFile putanja
    let sveLinije = lines sadrzaj
    let parovi = zip [1..] sveLinije
    let rezultat = [broj | (broj, linija) <- parovi, proveriUparenost linija]
    mapM_ print rezultat
    
proveriUparenost :: String -> Bool
proveriUparenost linija = proveri [] linija
    where
        -- kraj linije: ako je stek prazan vrati True, inace False
        proveri [] [] = True
        proveri _ [] = False
        -- kada se otvorena zagrada sa steka i zatvorena iz linije poklope, skidamo ih
        proveri ('(' : ostatakSteka) (')' : ostatakLinije) = proveri ostatakSteka ostatakLinije
        proveri ('[' : ostatakSteka) (']' : ostatakLinije) = proveri ostatakSteka ostatakLinije
        proveri ('{' : ostatakSteka) ('}' : ostatakLinije) = proveri ostatakSteka ostatakLinije
        -- ako naidjemo na zatvorenu zagradu koja se ne poklapa sa vrhom steka
        proveri _ (')' : _) = False
        proveri _ (']' : _) = False
        proveri _ ('}' : _) = False
        -- otvorene zagrade idu direktno na stek
        proveri stek (c : ostatakLinije)
            | c `elem` "([{" = proveri (c : stek) ostatakLinije
        -- sve ostale karaktere (slova, brojeve, razmake/beline) ignorisemo
        proveri stek (_ : ostatakLinije) = proveri stek ostatakLinije
