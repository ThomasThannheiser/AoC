import AoCHelper (split')
import MPCAS (Parser, runParser, char)
import Data.Functor (($>))
import Control.Applicative (Alternative ((<|>)))

data Rule = Val Char 
          | And Rule Rule 
          | Or Rule Rule 
          | See Int 
          deriving Show

genParser :: Rule -> [(Int, Rule)] -> Parser [] ()
genParser (Val c)   _     = char c $> ()
genParser (And x y) rules = genParser x rules  *> genParser y rules
genParser (Or x y)  rules = genParser x rules <|> genParser y rules
genParser (See n)   rules = genParser x rules
  where Just x = lookup n rules

matched :: [] ((), String) -> Bool
matched xs = any (null . snd) xs && (not . null $ xs)  
        
toRule :: [[String]] -> Rule
toRule [["\"a\""]]  = Val 'a'
toRule [["\"b\""]]  = Val 'b'
toRule [[x]]    = See $ read x
toRule [x : xs] = And (See $ read x) (toRule [xs])
toRule [xs, ys] = Or (toRule [xs]) (toRule [ys])

parseRule :: String -> (Int, Rule)
parseRule s = (read nr, toRule rs)
  where rs = map words . split' (== '|') $ r
        (nr, ':' : ' ' : r) = span (/= ':') s

day19 input = length . filter matched . map (runParser $ genParser (See 0) rulesMap) $ messages
  where rulesMap = map parseRule rules
        (rules, "" : messages) = break null input
        
main = do
  input1 <- readFile "day19.input"
  input2 <- readFile "day19_2.input"
  mapM_ (print . day19 . lines) [input1, input2]

-- 176
-- 352