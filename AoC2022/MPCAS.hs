{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MPCAS where

import Data.Char (isUpper, isLower, isSpace, isDigit, isAlpha, isAlphaNum)
import Control.Applicative (Alternative ((<|>), empty, many, some))
import Control.Monad.Trans.State.Lazy (StateT (StateT, runStateT))
import Control.Monad (MonadPlus)

newtype Parser m a = Parser { unParser :: StateT String m a }
        deriving (Functor, Applicative, Alternative, Monad)

runParser :: Parser m a -> String -> m (a, String)
runParser = runStateT . unParser

{-- simplest parser combinators --}

anyChar :: Alternative m => Parser m Char
anyChar = Parser . StateT $ \case
  []       -> empty
  (c : cs) -> pure (c, cs)

satisfy :: Alternative m => (Char -> Bool) -> Parser m Char
satisfy pred = Parser . StateT $ \case
  (c : cs) | pred c -> pure (c, cs)
  _                 -> empty  

char :: Alternative m => Char -> Parser m Char
char = satisfy . (==)

lower, upper, letter, alphanum, digit :: Alternative m => Parser m Char
lower    = satisfy isLower
upper    = satisfy isUpper
letter   = satisfy isAlpha
alphanum = satisfy isAlphaNum
digit    = satisfy isDigit

string :: (Alternative m, Monad m) => String -> Parser m String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

ident :: MonadPlus m => Parser m String
ident = (:) <$> lower <*> many alphanum

nat, int :: MonadPlus m => Parser m Integer
nat = read <$> some digit
int = (negate <$ char '-' <|> pure id) <*> nat

intP :: MonadPlus m => Parser m Int
intP = fromInteger <$> int
  
{-- separating and chaining --}

sepBy, sepBy1 :: MonadPlus m => Parser m a -> Parser m b -> Parser m [a]
sepBy  p sep = (p `sepBy1` sep) <|> pure []
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

chainl, chainr :: MonadPlus m => Parser m a -> Parser m (a -> a -> a) -> a -> Parser m a
chainl p op a = (p `chainl1` op) <|> pure a
chainr p op a = (p `chainr1` op) <|> pure a

chainl1, chainr1 :: MonadPlus m => Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 p op = foldl (flip id) <$> p <*> many (flip <$> op <*> p) 
chainr1 p op = flip (foldr id) <$> many (flip id <$> p <*> op) <*> p
 
{-- space and tokenizing --}

space :: MonadPlus m => Parser m String
space = many $ satisfy isSpace

token :: MonadPlus m => Parser m a -> Parser m a
token p = space *> p <* space

symbol :: MonadPlus m => String -> Parser m String
symbol = token . string

identifier :: MonadPlus m => Parser m String
identifier = token ident

natural, integer ::  MonadPlus m =>  Parser m Integer
natural = token nat
integer = token int

embraced :: MonadPlus m => Char -> Char -> Parser m a -> Parser m a
embraced open close p = char open *> p <* char close

parenthezised, bracketed, taged, curlied :: MonadPlus m => Parser m a -> Parser m a
parenthezised = embraced '(' ')'
bracketed     = embraced '[' ']'
curlied       = embraced '{' '}'
taged         = embraced '<' '>'
