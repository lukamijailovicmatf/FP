-- Napisati program koji ispisuje prvih pet linija tekstualne datoteke čije ime je navedeno kao prvi argument komandne linije (ovo je takozvani head program). 
-- Ako argument nije prisutan, program treba da ispiše poruku o grešci. Ako je prisutan drugi argument, proveriti da li taj argument predstavlja prirodan broj, 
-- i u potvrdnom slučaju ispisati broj linija u skladu sa tim argumentom.

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    argumenti <- getArgs
    case argumenti of
        [] -> putStrLn ("Greska. Nije navedena putanja do datoteke.")
        [putanja] -> ispisiLinije putanja 5
        [putanja, strN] -> case parsirajPrirodanBroj strN of
            Just n -> ispisiLinije putanja n
            Nothing -> putStrLn ("Greska. Drugi argument mora biti prirodan broj.")
        _ -> putStrLn ("Greska. Previse argumenata komandne linije.")
        
parsirajPrirodanBroj :: String -> Maybe Int
parsirajPrirodanBroj s = case readMaybe s of
    Just n | n >= 0 -> Just n
    _ -> Nothing
    
ispisiLinije :: FilePath -> Int -> IO ()
ispisiLinije putanja n = do
    sadrzaj <- readFile putanja
    let sveLinije = lines sadrzaj
        prveLinije = take n sveLinije
    putStr (unlines prveLinije)
