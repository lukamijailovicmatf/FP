-- Kreirati tip Racun sa poljima vlasnik (tekst), brojRacuna (tekst) i stanje (broj). Napisati funkcije uplati (dodaje novac na racun) i isplati
-- (skida novac sa racina ako ima dovoljno sredstava, u suprotnom vraca neizmenjen racun uz poruku).

-- module Main where

data Racun = Racun { vlasnik :: String,
                     brojRacuna :: String,
                     stanje :: Double
                   } deriving (Show, Eq)
                   
uplati :: Double -> Racun -> Racun
uplati iznos r = r {stanje = stanje r + iznos}

isplati :: Double -> Racun -> Racun
isplati iznos r
    | iznos <= stanje r = r {stanje = stanje r - iznos}
    | otherwise = r
    
{-
main :: IO ()
main = do
    let mojRacun = Racun {vlasnik = "Luka Mijailovic", brojRacuna = "160-123456-77", stanje = 1000.0}
        posleUplate = uplati 500.0 mojRacun
        posleIsplate = isplati 300.0 mojRacun
        propalaIsplata = isplati 5000.0 mojRacun
    print mojRacun
    print posleUplate
    print posleIsplate
    print propalaIsplata
-}
