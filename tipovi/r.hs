-- Treba napraviti klasu R, koja u sebi sadrzi funkciju rev:: a -> a
-- # instancirati tu klasu nad listama, tako da funkcija rev obrce listu (npr. rev [1,2,3] -> [3,2,1])
-- # instancirati tu klasu nad Integer, tako da funkcija rev obrce broj (npr. rev -123 -> -321)
-- # instancirati tu klasu nad parom (tuple), pri cemu oba elemnta tog para moraju biti u klasi R, tako da 
-- funkcija rev obrce elemente tog para (npr. rev (123, "neki")-> (321, "iken"))

class R a where
    rev :: a -> a
    
instance R [a] where
    rev lst = reverse lst
    
instance R Integer where
    rev broj = signum broj * read (reverse (show (abs broj)))
    
instance (R a, R b) => R (a, b) where
    rev (prvi, drugi) = (rev prvi, rev drugi)
