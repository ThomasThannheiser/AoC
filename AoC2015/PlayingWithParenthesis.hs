import MPCAS (Parser, runParser, parenthezised)
import Control.Applicative (Alternative ((<|>)))

data Tree = Nil
          | Bin Tree Tree
          deriving Show

foldparens :: (a -> a -> a) -> a -> Parser Maybe a -> Parser Maybe a
foldparens f e p = f <$> parenthezised p <*> p <|> pure e

nested :: Parser Maybe Tree
nested = foldparens Bin Nil nested

depth :: Parser Maybe Int 
depth = foldparens (max . (1+)) 0 depth

main = do
    input <- readFile "1_2015.txt"
    print . maybe Nil fst $ runParser nested input
    print . maybe 0 fst $ runParser depth input
