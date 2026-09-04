-- Treba napraviti klasu Filterable f i 2 funkcije
-- filterBy f :: (a -> Bool) -> f a -> f a
-- showIf f :: (Show a) => (a -> Bool) -> f a -> String
-- Treba da se instancira za listu, Maybe i Either.

class Filterable f where
    filterBy :: (a -> Bool) -> f a -> f a
    showIf :: (Show a) => (a -> Bool) -> f a -> String
    
instance Filterable [] where
    filterBy p lst = filter p lst
    showIf p lst = show (filterBy p lst)
    
instance Filterable Maybe where
    filterBy p (Just x)
        | p x = Just x
        | otherwise = Nothing
    filterBy p Nothing = Nothing
    showIf p mx = show (filterBy p mx)
    
instance (Monoid e, Show e) => Filterable (Either e) where
    filterBy p (Right x)
        | p x = Right x
        | otherwise = Left mempty
    filterBy p (Left e) = Left e
    showIf p ex = show (filterBy p ex)
