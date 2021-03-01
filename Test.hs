module Test where

import MPCAS(Parser, runParser, char, natural, chainl1, chainr1, parenthezised)
import Control.Applicative ((<|>))

expr, expr' :: Parser Maybe Int
expr  = foldl chainl1 (factor expr)  [minus, expo]
expr' = foldl chainr1 (factor expr') [minus, expo]

factor :: Parser Maybe Int -> Parser Maybe Int
factor expr = natural <|> parenthezised expr

minus, expo :: Parser Maybe (Int -> Int -> Int)
minus = (-) <$ char '-'
expo  = (^) <$ char '^'

main = do
    mapM_ (print . ($ "1-2-3-4-(5-6)") . runParser ) [expr, expr']
    mapM_ (print . ($ "2^3^2") . runParser) [expr, expr']
    mapM_ (print . ($ "2^2^2^2") . runParser) [expr, expr']