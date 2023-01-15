module AoC2015Day6 where

import MPCAS (Parser, runParser, char, natural, symbol)
import Data.Set as S (Set, fromList, union, difference, empty, insert)
import Data.Foldable (asum)

type Point = (Int, Int)

data Instruction
  = ON
  | OFF
  | TOGGLE


instructionParser :: (Instruction, String) -> Parser Maybe Instruction
instructionParser (i, s) = i <$ symbol s

instructonStrings :: [(Instruction, String)]
instructonStrings = [(ON, "turn on"), (OFF, "turn off"), (TOGGLE, "toggle")]

pointParser :: Parser Maybe Point
pointParser = (,) <$> natural <* char ',' <*> natural

extractInstruction :: Parser Maybe (Instruction, Point, Point)
extractInstruction = do
  instruction <- asum (map instructionParser instructonStrings)
  left <- pointParser
  symbol "through"
  right <- pointParser
  return (instruction, left, right)

parseInstruction :: String -> (Instruction, Point, Point)
parseInstruction = maybe (OFF, (-1, -1), (-1, -1)) fst . runParser extractInstruction


corners2Points :: (Point, Point) -> [Point]
corners2Points (left, right) = [(x, y) | x <- [fst left .. fst right], y <- [snd left ..snd right]]

corners2PointSet :: (Point, Point) -> Set Point
corners2PointSet = fromList . corners2Points

instruction2Set :: Set Point -> (Instruction, Point, Point) -> Set Point
instruction2Set points (instruction, left, right) = 
  let newPoints = corners2PointSet (left, right) in 
  case instruction of
    ON -> points `union` newPoints
    OFF -> difference points newPoints
    TOGGLE -> difference points newPoints `union` difference newPoints points


day6_1 :: [String] -> Int
day6_1 = length . Prelude.foldl instruction2Set S.empty . Prelude.map parseInstruction

day6_2 = map parseInstruction

{-- solved with SQLite and Excel! --}
{-- also solved in Elm! --}

-- 543903
-- 14687245

main = do
   print . day6_1 . lines =<< readFile "6_2015.txt"