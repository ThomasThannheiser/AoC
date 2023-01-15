module Day11 where

import AoCHelper (Pair, both, chunksOf, splitBy)
import Control.Applicative (some, (<|>))
import Data.Bool (bool)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe (fromMaybe)
import MPCAS (Parser, anyChar, digit, int, runParser, sepBy, symbol, token, intP)

data MonkeyState = MonkeyState
  { monkey :: Int,
    items :: [Integer],
    calcCWL :: Integer -> Integer,
    divisor :: Integer,
    ftMonkeys :: Pair Int
  }

emptyMS :: MonkeyState
emptyMS = MonkeyState 0 [] id 1 (-1, -1)

clearItems :: MonkeyState -> MonkeyState
clearItems ms = ms {items = []}

addItems :: [Integer] -> MonkeyState -> MonkeyState
addItems toAdd ms = ms {items = items ms ++ toAdd}

monkeyP :: Parser Maybe Int
monkeyP = symbol "Monkey" *> intP <* symbol ":"

itemsP :: Parser Maybe [Integer]
itemsP = symbol "Starting items:" *> sepBy int (symbol ",")

calcCWLP :: Parser Maybe (Integer -> Integer)
calcCWLP = h <$> (symbol "Operation: new = old" *> expr)

expr :: Parser Maybe String
expr = (:) <$> token anyChar <*> (symbol "old" <|> some digit)

h :: String -> (Integer -> Integer)
h [] = id
h (x : xs) = case x of
  '*' -> if xs == "old" then (^ 2) else (* read xs)
  '+' -> if xs == "old" then (* 2) else (+ read xs)
  _ -> id

divisorP :: Parser Maybe Integer
divisorP = symbol "Test: divisible by" *> int

ftMonkey :: Bool -> Parser Maybe Int
ftMonkey b = symbol ("If " ++ bool "false" "true" b ++ ": throw to monkey") *> intP

block :: Parser Maybe MonkeyState
block = do
  m <- monkeyP
  i <- itemsP
  o <- calcCWLP
  d <- divisorP
  tm <- ftMonkey True
  fm <- ftMonkey False
  return $ MonkeyState m i o d (fm, tm)

parseBlock :: [String] -> MonkeyState
parseBlock = maybe emptyMS fst . runParser block . unlines

g :: (Integer -> Integer) -> Integer -> Pair Int -> Integer -> (Int, Integer)
g op d (f, t) n = (bool f t (cwl `mod` d == 0), cwl)
  where cwl = op n

f :: (Integer -> Integer) -> MonkeyState -> [(Int, [Integer])]
f addOnOp ms = map ((,) <$> fst . head <*> map snd) . groupBy ((==) `on` fst) . sort
             . map (g (addOnOp . calcCWL ms) (divisor ms) (ftMonkeys ms)) $ items ms

turn :: (Integer -> Integer) -> Int -> [MonkeyState] -> [MonkeyState]
turn addOnOp n mss = map h mss
  where toMonkeys = f addOnOp (mss !! n)
        h ms'
         | monkey ms' == n = clearItems ms'
         | otherwise = addItems (fromMaybe [] (lookup (monkey ms') toMonkeys)) ms'

day11 :: Bool -> Int -> [String] -> Int
day11 takeProduct rounds input = 
  product . take 2 . reverse . sort
    . foldl1 (zipWith (+)) . map itemsPerRound . chunksOf monkeys . init
    . scanl (flip (turn addOnOp)) start
    . take (rounds * monkeys) . cycle $ [0..monkeys -1]
  where
    start = map parseBlock . splitBy (== "") $ input
    monkeys = length start
    addOnOp = bool (`div` 3) (`mod` (product . map divisor $ start)) takeProduct
    itemsPerRound = zipWith (\n -> length . items . (!! n)) [0..monkeys -1]

day11_1, day11_2 :: [String] -> Int
day11_1 = day11 False 20
day11_2 = day11 True 10000

-- 112815
-- 25738411485

main :: IO ()
main = do
  input <- readFile "day11.input"
  print . day11_1 . lines $ input
  print . day11_2 . lines $ input