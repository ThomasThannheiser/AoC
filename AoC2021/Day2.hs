module Day2 where

import MPCAS (Parser, runParser, natural, symbol)
import Data.Foldable (asum)

data CMD = UP | DOWN | FORWARD deriving (Show)

commands :: [(String, CMD)]
commands = [("up", UP), ("down", DOWN), ("forward", FORWARD)]

makeCmdParser :: (String, CMD) -> Parser Maybe CMD
makeCmdParser (str, cmd) = cmd <$ symbol str

command :: Parser Maybe CMD
command = asum $ map makeCmdParser commands

extract :: Parser Maybe (CMD, Int)
extract = (,) <$> command <*> natural

parse :: String -> (CMD, Int)
parse = maybe (FORWARD, 0) fst . runParser extract


interpret1 :: (Int, Int) -> (CMD, Int) -> (Int, Int)
interpret1 (h, v) (cmd, delta) = case cmd of
  UP      -> (h, v - delta)
  DOWN    -> (h, v + delta)
  FORWARD -> (h + delta, v)

interpret2 :: (Int, Int, Int) -> (CMD, Int) -> (Int, Int, Int)
interpret2 (h, v, aim) (cmd, delta) = case cmd of
  UP      -> (h, v, aim - delta)
  DOWN    -> (h, v, aim + delta)
  FORWARD -> (h + delta, v + delta * aim, aim)

day2_1, day2_2 :: [String] -> Int
day2_1 input = h * v
  where (h, v) = foldl interpret1 (0, 0) $ map parse input
day2_2 input = h * v
  where (h, v, _) = foldl interpret2 (0, 0, 0) $ map parse input

-- 1815044
-- 1739283308

main = do
  print . day2_1 . lines =<< readFile "day2.input"
  print . day2_2 . lines =<< readFile "day2.input"
