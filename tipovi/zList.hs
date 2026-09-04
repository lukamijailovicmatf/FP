-- Napraviti tip podataka ZList a koji prima listu kao argument, napraviti konstruktor pod istim imenom i napraviti getList koji dohvata tu listu.
-- # instancirati show tako da ispis bude ZList [1..5] = [<1, 2, 3, 4, 5>]
-- # instancirati Funktor
-- # instancirati Aplikativ
-- # ako moze instancirati Monadu, ako ne, obrazloziti

import Data.List (intercalate)

data ZList a = ZList { getList :: [a] }

instance Show a => Show (ZList a) where
    show (ZList elementi) = "[<" ++ spoj elementi ++ ">]"
        where spoj xs = intercalate ", " (map show xs)
        
instance Functor ZList where
    fmap f (ZList elementi) = ZList (map f elementi)
    
instance Applicative ZList where
    pure vrednost = ZList (repeat vrednost)
    (ZList funkcije) <*> (ZList elementi) = ZList (zipWith ($) funkcije elementi)
