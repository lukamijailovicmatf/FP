-- Napisati program koji sa standardnog ulaza učitava reč s i u datoteku rotacije.txt upisuje sve njene rotacije. 
-- Pretpostaviti da je maksimalna dužina reči 20 karaktera. U slučaju greške, ispisatiodgovarajuću poruku.

module Main where

main :: IO ()
main = do
    putStrLn ("Unesite rec:")
    rec <- getLine
    let sveRotacije = rotacije rec
    let ispis = unlines sveRotacije
    writeFile "rotacije.txt" ispis
    
rotacije :: String -> [String]
rotacije [] = []
rotacije xs = take (length xs) (iterate rotiraj xs)
    where rotiraj [] = []
          rotiraj (y:ys) = ys ++ [y]
