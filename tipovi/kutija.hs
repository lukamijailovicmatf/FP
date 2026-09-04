-- Definisati tip Box koji moze biti prazan ili neprazan, kada je neprazan ima vrednost i neki string. 
-- Instancirati Show tako da ukoliko nije prazan prikaze vrednost, a u suprotnom ispisuje </>. 
-- Instancirati Functor i Applicative (pure i bind) nad ovim tipom.

data Box a = Prazan
           | Neprazan a String
           
instance Show a => Show (Box a) where
    show Prazan = "</>"
    show (Neprazan x _) = show x
    
instance Functor Box where
    fmap f Prazan = Prazan
    fmap f (Neprazan x s) = Neprazan (f x) s
    
instance Applicative Box where
    pure x = Neprazan x ""
    Prazan <*> _ = Prazan
    _ <*> Prazan = Prazan
    (Neprazan f s1) <*> (Neprazan x s2) = Neprazan (f x) (s1 ++ s2)
    
instance Monad Box where
    Prazan >>= _ = Prazan
    (Neprazan x s1) >>= f = case f x of
                                Prazan -> Prazan
                                Neprazan y s2 -> Neprazan y (s1 ++ s2)
