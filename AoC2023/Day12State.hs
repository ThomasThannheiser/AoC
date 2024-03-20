module Day12 where

import AoCHelper (readIntLst, splitWith)

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.State.Lazy (StateT, evalState, get, modify)

type CondRec = (String, [Int])

type Cache = Map CondRec Int

day12_1, day12_2 :: [String] -> Int
day12_1 = sum . map (run . parsePatterns)
day12_2 = sum . map (run . f . parsePatterns)
  where f (xs, ns) = (intercalate "?" $ replicate 5 xs, concat $ replicate 5 ns)

run :: CondRec -> Int
run cr = evalState (substitute' cr) Map.empty

substitute :: CondRec -> StateT Cache Identity Int
substitute ("", ns) = return . fromEnum $ null ns
substitute (xs, []) = return . fromEnum $ '#' `notElem` xs
substitute ('.' : xs, n : ns) = substitute' (dropWhile (== '.') xs, n : ns)
substitute ('?' : xs, n : ns) =
  (+) <$> substitute' ('#' : xs, n : ns) <*> substitute' ('.' : xs, n : ns)
substitute ('#' : xs, n : ns)
  | length xs < n - 1 = return 0
  | '.' `elem` take (n - 1) xs = return 0
  | length xs == (n - 1) = substitute ("", ns)
  | xs !! (n - 1) == '#' = return 0
  | otherwise = substitute' (drop n xs, ns)

substitute' :: CondRec -> StateT Cache Identity Int
substitute' cr = do
  c <- get
  case Map.lookup cr c of
    Just n -> return n
    Nothing -> do
      n <- substitute cr
      modify (Map.insert cr n)
      return n

parsePatterns :: String -> CondRec
parsePatterns = ((,) <$> fst <*> readIntLst . snd) . splitWith ' '

-- 6935
-- 3920437278260

main :: IO ()
main = do
  input <- readFile "day12.input"
  print . day12_1 . lines $ input
  print . day12_2 . lines $ input
