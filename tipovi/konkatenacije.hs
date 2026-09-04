-- Napraviti klasu Concatenatable t koja ima funkciju concatenate :: t -> t -> t. Instancirati ovu klasu za liste, 
-- integere (pisati Integer a ne Int) (3 concatenate 2 = 32), uredjene parove ((1, 2) concatenate (3, 4) = (13, 24)) i za Maybe.

class Concatenatable t where
    concatenate :: t -> t -> t
    
instance Concatenatable [a] where
    concatenate lista1 lista2 = lista1 ++ lista2
    
instance Concatenatable Integer where
    concatenate broj1 broj2 = read (show broj1 ++ show broj2)
    
instance (Concatenatable a, Concatenatable b) => Concatenatable (a, b) where
    concatenate (x1, y1) (x2, y2) = (concatenate x1 x2, concatenate y1 y2)
    
instance Concatenatable a => Concatenatable (Maybe a) where
    concatenate (Just x) (Just y) = Just (concatenate x y)
    concatenate (Just x) Nothing = Just x
    concatenate Nothing (Just y) = Just y
    concatenate Nothing Nothing = Nothing
