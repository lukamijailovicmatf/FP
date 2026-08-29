-- Definisati tip Point sa konstruktorom Point koji prima koordinate tacke x i y i napisati funkciju koja vraca True ako je uneta tacka na x ili y osi i False ako nije.

data Point = Point Double Double
    deriving (Show)
    
naOsi :: Point -> Bool
naOsi (Point x y) = x == 0 || y == 0

{-
main :: IO ()
main = do
    let t1 = Point 0.0 5.0
        t2 = Point 3.0 0.0
        t3 = Point 0.0 0.0
        t4 = Point 4.0 2.0
    putStrLn ("Tacka (0,5) je na nekoj od osa: " ++ show (naOsi t1))
    putStrLn ("Tacka (3,0) je na nekoj od osa: " ++ show (naOsi t2))
    putStrLn ("Tacka (0,0) je na nekoj od osa: " ++ show (naOsi t3))
    putStrLn ("Tacka (4,2) je na nekoj od osa: " ++ show (naOsi t4))
-}
