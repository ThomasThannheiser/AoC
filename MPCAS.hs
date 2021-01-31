{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MPCAS where

import Data.Char (isUpper, isLower, isSpace, isDigit, isAlpha, isAlphaNum)
import Control.Applicative (Alternative ((<|>), empty, many, some))
import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT))

newtype Parser a = Parser { unParser :: StateT String Maybe a }
        deriving (Functor, Applicative, Alternative, Monad)

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT . unParser

{-- simplest parser combinators --}

anyChar :: Parser Char
anyChar = Parser . StateT $ \case
    []       -> empty
    (c : cs) -> pure (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    c <- anyChar 
    if pred c then pure c else empty

char :: Char -> Parser Char
char = satisfy . (==)

lower, upper, letter, alphanum :: Parser Char
lower = satisfy isLower
upper = satisfy isUpper
letter = satisfy isAlpha
alphanum = satisfy isAlphaNum

string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

ident :: Parser String
ident = (:) <$> lower <*> many alphanum
    
digit :: Parser Char
digit = satisfy isDigit

nat, int :: Parser Int
nat = read <$> some digit
int = (char '-' *> (negate <$> nat)) <|> nat
    
{-- separating and chaining --}

sepBy, sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = scan where
  scan = flip ($) <$> p <*> rst
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
 
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainr1` op) <|> pure a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan where
  scan = flip ($) <$> p <*> rst
  rst = flip <$> op <*> scan <|> pure id
 
{-- space and tokenizing --}

space :: Parser String
space = many $ satisfy isSpace

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol = token . string

identifier :: Parser String
identifier = token ident

natural, integer ::  Parser Int
natural = token nat
integer = token int

parenthezised :: Parser a -> Parser a
parenthezised p = char '(' *> p <* char ')'