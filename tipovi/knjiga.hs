-- Napraviti tip Book koji sadrzi naziv knjige, autora, broj strana i da li je izdata. Napisati funkciju isAvailable :: Book -> Bool, koja vraca False 
-- ako je knjiga izdata, a True ako knjiga nije izdata.

data Book = Book { naziv :: String,
                   autor :: String,
                   brojStrana :: Int,
                   izdata :: Bool
                 } deriving (Show)
                 
isAvailable :: Book -> Bool
isAvailable knjiga = not (izdata knjiga)
