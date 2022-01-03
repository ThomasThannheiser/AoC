module Day13 where

import MPCAS (Parser, runParser, char, symbol, natural)
import AoCHelper (split')
import Data.Set (fromList, toList)
import Control.Applicative (Alternative ((<|>)))

type Point = (Int, Int)
type Fold = (Char, Int)

extractFolds :: Parser Maybe Fold
extractFolds = (,) <$> (symbol "fold along" *> (char 'x' <|> char 'y')) <*> (char '=' *> natural)

parseFolds :: String -> Fold
parseFolds = maybe ('z', -1) fst . runParser extractFolds 

extractPoints :: Parser Maybe Point
extractPoints = (,) <$> natural <* char ',' <*> natural 

parsePoints :: String -> Point
parsePoints = maybe (-1, -1) fst . runParser extractPoints

xFaltung, yFaltung :: Int -> Point -> Point
xFaltung x (a, b) = (2 * x - a, b)
yFaltung y (a, b) = (a, 2 * y - b)

faltung :: Fold -> [Point] -> [Point]
faltung (c, v) points = toList . fromList $ (noOp ++ map (f v) op)
  where
    noOp = filter (\p -> g p < v) points
    op = filter (\p -> g p > v) points
    f = if c == 'x' then xFaltung else yFaltung
    g = if c == 'x' then fst else snd

day13 :: [String] -> ([Point], [Fold])
day13 input = (points, folds)
  where
    folds = map parseFolds flds
    points = map parsePoints pts
    [pts, flds] = split' null input 
 
day13_1 :: [String] -> Int
day13_1 input = length . faltung (head folds) $ points
  where (points, folds) = day13 input

day13_2 :: [String] -> String
day13_2 input = out $ foldr faltung points (reverse folds)
  where (points, folds) = day13 input

out :: [Point] -> String
out points = unlines . map f $ [0 .. maxy]
  where 
    f y = map (\x -> if (x, y) `elem` points then '#' else ' ') [0 .. maxx]
    maxx = maximum . map fst $ points
    maxy = maximum . map snd $ points

-- 775
-- REUPUPKR


main = do
    print . day13_1 . lines =<< readFile "day13.input"
    putStr . day13_2 . lines =<< readFile "day13.input"