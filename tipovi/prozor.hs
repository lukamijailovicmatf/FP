-- Definisati tip Prozor, identifikator (int), naziv (string), sadrzaj (a). Mkprozor, gettere.
-- Ord nad Prozor za poredjenje na osnovu identifikatora, funktor i monadu.
 
 data Prozor a = MkProzor { identifikator :: Int,
                            naziv :: String,
                            sadrzaj :: a
                          } deriving (Show)
        
instance Eq (Prozor a) where
    p1 == p2 = identifikator p1 == identifikator p2
    
instance Ord (Prozor a) where
    compare p1 p2 = compare (identifikator p1) (identifikator p2)
    
instance Functor Prozor where
    fmap f (Prozor id n s) = Prozor id n (f s)
    
instance Applicative Prozor where
    pure x = Prozor 0 "" x
    (Prozor i1 n1 f) <*> (Prozor i2 n2 x) = Prozor (i1 + i2) (n1 ++ n2) (f x)
    
instance Monad Prozor where
    (Prozor i1 n1 x) >>= f = 
        let Prozor i2 n2 y = f x
        in Prozor (i1 + i2) (n1 ++ n2) y
