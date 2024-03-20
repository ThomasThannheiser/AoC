module Day12 where

import AoCHelper (readIntLst, splitWith)

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map

type CondRec = (String, [Int])

type Cache = Map CondRec Int

day12_1, day12_2 :: [String] -> Int
day12_1 = sum . map (run . parsePatterns)
day12_2 = sum . map (run . f . parsePatterns)
  where f (xs, ns) = (intercalate "?" $ replicate 5 xs, concat $ replicate 5 ns)

run :: CondRec -> Int
run = fst . (`substitute'` Map.empty)

substitute :: CondRec -> Cache -> (Int, Cache)
substitute ("", ns) c = (fromEnum $ null ns, c)
substitute (xs, []) c = (fromEnum $ '#' `notElem` xs, c)
substitute ('.' : xs, n : ns) c = substitute' (dropWhile (== '.') xs, n : ns) c
substitute ('?' : xs, n : ns) c =
  let (x, c') = substitute' ('#' : xs, n : ns) c
      (y, c'') = substitute' ('.' : xs, n : ns) c'
   in (x + y, c'')
substitute ('#' : xs, n : ns) c
  | length xs < n - 1 = (0, c)
  | '.' `elem` take (n - 1) xs = (0, c)
  | length xs == (n - 1) = substitute' ("", ns) c
  | xs !! (n - 1) == '#' = (0, c)
  | otherwise = substitute' (drop n xs, ns) c

substitute' :: CondRec -> Cache -> (Int, Cache)
substitute' cr c =
  case Map.lookup cr c of
    Just n -> (n, c)
    Nothing -> let (m, c') = substitute cr c 
                in (m, Map.insert cr m c')

parsePatterns :: String -> CondRec
parsePatterns = ((,) <$> fst <*> readIntLst . snd) . splitWith ' '

-- 6935
-- 3920437278260

main :: IO ()
main = do
  input <- readFile "day12.input"
  print . day12_1 . lines $ input
  print . day12_2 . lines $ input
