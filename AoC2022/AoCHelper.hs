module AoCHelper where
  
import Data.List (unfoldr, tails)
import Data.Bifunctor (bimap)

type Pair a = (a, a)

between :: Int -> (Int, Int) -> Bool
between value (min, max) = min <= value && value <= max

bin2Int :: [Bool] -> Int
bin2Int = foldr (\x y -> fromEnum x + 2 * y) 0 . reverse

iter :: Int -> (a -> a) -> (a -> a)
iter n = foldr (.) id . replicate n

readIntLst :: String -> [Int]
readIntLst s = read $ '[' : s ++ "]"

(.+.) :: Pair Int -> Pair Int -> Pair Int
(.+.) (x, y) = bimap (+ x) (+ y)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr $ \xs -> if null xs then Nothing else Just $ splitAt n xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = unfoldr f
  where f [] = Nothing
        f (x : xs) = Just $ break p (if p x then xs else x : xs)

splitWith :: Char -> String -> (String, String)
splitWith c s = let (a, _:b) = break (== c) s in (a, b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

windowed :: Int -> [a] -> [[a]]
windowed n = filter ((== n) . length) . map (take n) . tails

(.-.) :: Pair Int -> Pair Int -> Pair Int
(.-.) (x, y) = (.+.) (x, y) . both negate 