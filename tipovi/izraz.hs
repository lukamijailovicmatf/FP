-- Napraviti tip aritmeticki izraz koji moze biti varijabla, konstanta, mnozenje i sabiranje. Instancirati funktor tako da primeni fmap f na konstante. 
-- Napisati funkciju eval koja dobije listu ("varname", vrednost) i izraz i evaluira za date vrednosti, ako neka var nema vrednost podrazumevati 1.

module Main where

data Izraz a = Varijabla String
             | Konstanta a
             | Sabiranje (Izraz a) (Izraz a)
             | Mnozenje (Izraz a) (Izraz a)
             deriving (Show)
             
instance Functor Izraz where
    fmap f (Varijabla v) = Varijabla v
    fmap f (Konstanta c) = Konstanta (f c)
    fmap f (Sabiranje e1 e2) = Sabiranje (fmap f e1) (fmap f e2)
    fmap f (Mnozenje e1 e2) = Mnozenje (fmap f e1) (fmap f e2)
    
eval :: Num a => [(String, a)] -> Izraz a -> a
eval _ (Konstanta c) = c
eval okruzenje (Varijabla v) = 
    case lookup v okruzenje of
        Just val -> val
        Nothing -> 1
eval okruzenje (Sabiranje e1 e2) = eval okruzenje e1 + eval okruzenje e2
eval okruzenje (Mnozenje e1 e2) = eval okruzenje e1 * eval okruzenje e2

main :: IO ()
main = do
    -- (x + 5) * y
    let izraz = Mnozenje (Sabiranje (Varijabla "x") (Konstanta 5)) (Varijabla "y")
    let okruzenje = [("x", 10)]
    putStrLn ("Originalni izraz: " ++ show izraz)
    let rezultat1 = eval okruzenje izraz
    putStrLn ("Rezultat evaluacije za x = 10 i y = 1: " ++ show rezultat1)
    -- (x + 105) * 1
    let izrazUvecaneKonstante = fmap (+100) izraz
    putStrLn ("Izraz nakon primene fmap (+100): " ++ show izrazUvecaneKonstante)
    let izraz2 = eval okruzenje izrazUvecaneKonstante
    putStrLn ("Rezultat evaluacije novog izraza: " ++ show izraz2)
