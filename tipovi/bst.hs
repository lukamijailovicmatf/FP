-- Binarno stablo pretrage sa pomocnim funkcijama (addToTree, toList, fromList, treeMap, custom show, ...). 
-- Da li mogu da se testiraju fromList i toList koristeci quickcheck i da li moze da se instancira Functor.

data Drvo a = Prazno
            | Cvor a (Drvo a) (Drvo a)
            deriving (Eq)
          
addToTree :: Ord a => a -> Drvo a -> Drvo a
addToTree element Prazno = Cvor element Prazno Prazno
addToTree element (Cvor koren levo desno)
    | element < koren = Cvor koren (addToTree element levo) desno
    | element > koren = Cvor koren levo (addToTree element desno)
    | otherwise = Cvor koren levo desno
    
toList :: Drvo a -> [a]
toList Prazno = []
toList (Cvor koren levo desno) = toList levo ++ [koren] ++ toList desno

fromList :: Ord a => [a] -> Drvo a
fromList [] = Prazno
fromList lista = foldl (flip addToTree) Prazno lista

-- primena funkcije na svaki element u stablu
treeMap :: (a -> b) -> Drvo a -> Drvo b
treeMap _ Prazno = Prazno
treeMap f (Cvor koren levo desno) = Cvor (f koren) (treeMap f levo) (treeMap f desno)

searchTree :: Ord a => a -> Drvo a -> Bool
searchTree _ Prazno = False
searchTree trazeniElement (Cvor koren levo desno)
    | trazeniElement == koren = True
    | trazeniElement < koren = searchTree trazeniElement levo
    | otherwise = searchTree trazeniElement desno
