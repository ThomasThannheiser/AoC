import AoCHelper (split')
import MPCAS (Parser, runParser, char)
import Data.Functor (($>))
import Control.Applicative (Alternative(empty, some, (<|>)))
import Control.Monad (guard)

data Rule = Val Char 
          | And Rule Rule 
          | Or Rule Rule 
          | See Int 
          deriving Show

genParser :: Rule -> [(Int, Rule)] -> Parser ()
genParser (Val c)   _     = char c $> ()
genParser (And x y) rules = genParser x rules  *> genParser y rules
genParser (Or x y)  rules = genParser x rules <|> genParser y rules
genParser (See n)   rules = genParser x rules
  where Just x = lookup n rules

parser19_2 :: [(Int, Rule)] -> Parser ()
parser19_2 rulesMap = do 
  count42 <- some p42
  count31 <- some p31
  guard $ count42 > count31
  where p42 = genParser (See 42) rulesMap
        p31 = genParser (See 31) rulesMap

matched :: Maybe ((), String) -> Bool
matched = (== Just ((), []))
        
toRule :: [[String]] -> Rule
toRule [["\"a\""]]  = Val 'a'
toRule [["\"b\""]]  = Val 'b'
toRule [[x]]    = See (read x)
toRule [[x, y]] = And (See (read x)) (See (read y))
toRule [xs, ys] = Or (toRule [xs]) (toRule [ys])

parseRule :: String -> (Int, Rule)
parseRule s = (read nr, toRule rs)
  where rs = map words . split' (== '|') $ r
        (nr, ':' : ' ' : r) = span (/= ':') s

day19 parser input = length . filter matched . map (runParser $ parser rulesMap) $ messages
  where rulesMap = map parseRule rules
        (rules, "" : messages) = break null input
        
day19_1 = day19 $ genParser (See 0) 
day19_2 = day19 parser19_2

-- 176
-- 352

main = do
  input <- readFile "day19.input"
  print . day19_1 . lines $ input
  print . day19_2 . lines $ input