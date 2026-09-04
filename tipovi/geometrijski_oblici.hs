-- Definisati tip podataka Oblik koji moze biti Krug (definisan poluprecnikom) ili Pravougaonik (definisan duzinom i sirinom). Napisati funkcije povrsina i obim
-- koje izracunavaju povrsinu i obim za dati oblik.

-- module Main where

data Oblik = Krug Double
           | Pravougaonik Double Double
           deriving (Show, Eq)
           
povrsina :: Oblik -> Double
povrsina (Krug r) = r * r * pi
povrsina (Pravougaonik a b) = a * b

obim :: Oblik -> Double
obim (Krug r) = 2 * r * pi
obim (Pravougaonik a b) = 2 * (a + b)

{-
main :: IO ()
main = do
    let k = Krug 5.0
        p = Pravougaonik 4.0 6.0
    putStrLn ("Povrsina kruga: " ++ show (povrsina k))
    putStrLn ("Obim pravougaonika: " ++ show (obim p))
-}
