-- Napraviti tip CryptoCurrency koja ce da sadrzi vrednost te valute izrazeno u USD, kao i koliko se povecala/smanjila cena u poslednjih 24h izrazeno u procentima.
-- Napraviti tip CryptoWallet koja ce da cuva te CryptoCurrency, kolicinu koju korisnik ima, kao i kad je poslednji put menjao/kupio/koristio taj currency.

import Data.Time (UTCTime)

data CryptoCurrency = CryptoCurrency { vrednostUSD :: Double,
                                       promena24h :: Double
                                     } deriving (Show, Eq)
                                     
data CryptoWallet = CryptoWallet { valuta :: CryptoCureency,
                                   kolicina :: Double,
                                   vreme :: UTCTime
                                 } deriving (Show, Eq)
