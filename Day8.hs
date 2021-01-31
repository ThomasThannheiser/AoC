module Day8 where

type Instruction = (String, Int)
type Code = [Instruction]

parseCode :: String -> Instruction
parseCode code = (a, read . drop (if '+' `elem` b then 2 else 1) $ b)
  where (a, b) = span (/= ' ') code

eval :: (Int, Int) -> Instruction -> (Int, Int)
eval (line, acc) (command, value) = case command of
  "jmp"     -> (line + value, acc)
  "acc"     -> (line + 1, acc + value)
  _         -> (line + 1, acc)

calculate :: (Int, Int) -> [Int] -> Code -> (Bool, Int)
calculate (line, acc) done code
  | line `elem` done    = (True, acc)
  | line >= length code = (False, acc) 
  | otherwise = let (line', acc') = eval (line, acc) (code !! line)
                in calculate (line', acc') (line : done) code       

swapLine :: Instruction -> Instruction
swapLine (command, value) = case command of
  "nop"     -> ("jmp", value)
  "jmp"     -> ("nop", value)
  _         -> (command, value)

swapCode :: Code -> Int -> Code
swapCode code n = let (c1, c : c2) = splitAt n code in c1 ++ (swapLine c : c2)

calcSwapedCode :: Code -> Int -> (Bool, Int)
calcSwapedCode code n = if fst (code !! n) == "acc"
  then (True, 0)
  else calculate (0,0) [] (swapCode code n)

day8_1 = snd . calculate (0, 0) [] . map parseCode
day8_2 lst = snd . head . filter (not . fst) . map (calcSwapedCode code) $ [0..]
  where code = map parseCode lst

-- 1727
-- 552


main = do
  input <- readFile "day8.input"
  print . day8_1 . lines $ input
  print . day8_2 . lines $ input