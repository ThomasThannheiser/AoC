module Day4 where

import MPCAS (Parser, runParser, bracketed, char, identifier, natural)
import Data.List (group, sort, sortBy)
import Data.Char (ord, chr)
import Control.Applicative (Alternative (many))

{-- parsing --}

extractData :: Parser Maybe ([String], Int, [Char])
extractData =  (, ,) <$> many (identifier <* char '-') <*> natural <*> bracketed identifier 
    
parse :: String -> ([String], Int, [Char])
parse = maybe ([], 0, []) fst . runParser extractData


{-- Ceasar chiffre --}

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c = int2let (mod (let2int c + n) 26)


day4_1, day4_2 :: [String] -> Int
day4_1 = sum . map (\(xs, y, z) -> y) . filter (\(xs, y, z) -> xs == z) .
    map ((\(xs, y, z) -> (take 5 . map head . sortBy (\as bs -> compare (length bs) (length as)) .
    group . sort . concat $ xs, y, z)) . parse)

day4_2 = snd . head . filter (elem "northpole" . fst) . 
    map ((\(xs, y, _) -> (map (map (shift (mod y 26))) xs, y)) . parse)


-- 137896
-- 501

main = do
    print . day4_1 . lines =<< readFile "4_2016.txt"
    print . day4_2 . lines =<< readFile "4_2016.txt"