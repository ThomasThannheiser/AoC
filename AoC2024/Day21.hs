module Day21 where

import AoCHelper (Pair, (.-.))
import Data.Char (isDigit)

{-- day 21 --}

day21_1 input = sum $ map calc input
  where
    calc = (*) <$> read . filter isDigit . show <*> length . secondPadLevel
    secondPadLevel = padLevel . firstPadLevel
    firstPadLevel = padLevel . codeLevel
    padLevel pad = concat $ zipWith (curry (addA . inputDir)) ('A': pad) pad
    codeLevel code = concat $ zipWith (curry (addA . inputPad)) ('A': code) code
    addA = reverse . ('A':) . reverse

codePad :: Char -> Pair Int
codePad 'A' = (0, 2)
codePad '0' = (0, 1)
codePad '1' = (1, 0)
codePad '2' = (1, 1)
codePad '3' = (1, 2)
codePad '4' = (2, 0)
codePad '5' = (2, 1)
codePad '6' = (2, 2)
codePad '7' = (3, 0)
codePad '8' = (3, 1)
codePad '9' = (3, 2)

dirPad :: Char -> Pair Int
dirPad 'A' = (1, 2)
dirPad '^' = (1, 1)
dirPad '<' = (0, 0)
dirPad 'v' = (0, 1)
dirPad '>' = (0, 2)

inputPad :: Pair Char -> String
inputPad (from, to) = f $ codePad from .-. codePad to
  where
    f (x, y) = if from `elem` "0A"
      then ud x ++ lr y
      else lr y ++ ud x
    ud x = replicate (abs x) $ if x < 0 then '^' else 'v'
    lr y = replicate (abs y) $ if y < 0 then '>' else '<'

inputDir :: Pair Char -> String
inputDir (from, to) = f $ dirPad from .-. dirPad to
  where
    f (x, y) = if from `elem` "A^"
      then ud x ++ lr y
      else lr y ++ ud x
    ud x = replicate (abs x) $ if x < 0 then '^' else 'v'
    lr y = replicate (abs y) $ if y < 0 then '>' else '<'

-- example works, but should by coincidence

main :: IO ()
main = do
  input <- lines <$> readFile "input/21"
  print $ day21_1 input
  -- print $ day21_2 input  