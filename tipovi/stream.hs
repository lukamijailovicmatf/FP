-- Napisati tip Stream a koji ce primati beskonacno elemenata tipa a. Napisati funkcije fromList :: [a] -> Stream a, kao i toList :: Stream a -> [a].
-- Napisati i Show tako da ako gledamo da je ovo lista [1..100], da ispisuje samo prvih 10 elemenata, tj. [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...].
-- Instancirati Funktor i Applicative.

import Data.List (intercalate)

newtype Stream a = Stream [a]

fromList :: [a] -> Stream a
fromList xs = Stream xs

toList :: Stream a -> [a]
toList (Stream xs) = xs

instance Show a => Show (Stream a) where
    show (Stream xs) = 
        let prvih10 = take 10 xs
            tekstualno = map show prvih10
        in "[" ++ intercalate ", " tekstualno ++ ", ...]"
        
instance Functor Stream where
    fmap f (Stream xs) = Stream (map f xs)
    
instance Applicative Stream where
    pure x = Stream (repeat x)
    (Stream fs) <*> (Stream xs) = Stream (zipWith ($) fs xs)
