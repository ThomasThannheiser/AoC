module Day14 where

import AoCHelper (bin2Int)
import Data.List (isPrefixOf)
import Data.Map (toList, fromList)
import Data.Bits ((.|.), (.&.))

{-- parsing stuff --}

readData :: (a -> Bool) -> [a] -> [[a]]
readData _ [] = []
readData p (x : xs) = (x : a) : readData p b
  where (a, b) = break p xs

f lst = (last . words $ head lst, tail lst)

g :: String -> (Int, Int)
g s = (read . drop 4 . init . head $ lst, read . last $ lst)
  where lst = words s
  
h  (a, b) = ((intOr a, intAnd a), map g b)
h' (a, b) = ((intOr a, a),        map g b)

intOr  = bin2Int . map ((== '1') . xTo '0')
intAnd = bin2Int . map ((== '1') . xTo '1')

xTo b c  = if c == 'X' then b else c

 --}

noXToY c = if c == 'X' then 'X' else 'Y'
yToX   c = if c == 'Y' then 'X' else c

allBins 0 = [""]
allBins n = [x : y | x <- ['0', '1'], y <- allBins (n - 1)]

convert mask float = zipWith (:) float mask

maskToMasks mask = map (map yToX . (withoutX ++) . filter (/= 'X') . concat . convert newMask) floatings
  where floatings = allBins . length . filter (== 'X') $ mask
        newMask = readData (== 'X') startX
        (withoutX, startX) = span (/= 'X') . map noXToY $ mask

xMaskingFst (a, b) (c, d) = (a .|. c .&. b, d)
xMaskingSnd (a, b) (c, d) = (y, x) where (x, y) = xMaskingFst (a, b) (d, c)
maskingOrFst a (c, d) = (a .|. c, d)

orAndMaskSnd ((a, b), c) = map (xMaskingSnd (a, b)) c
orAndMaskFst ((a, b), c) = map (xMaskingFst (a, b)) c
orMask       ((a, b), c) = (b, map (maskingOrFst a) c)

calcMask b a  = orAndMaskFst ((intOr a, intAnd a), b)
calcMasks (a, b) = concatMap (calcMask b) . maskToMasks $ a

filterAndSum = sum . map snd . Data.Map.toList . Data.Map.fromList . concat

day14 maskCalc h = filterAndSum . map (maskCalc . h . f) . readData (isPrefixOf "mask = ")
day14_1 = day14 orAndMaskSnd h
day14_2 = day14 (calcMasks . orMask) h'

-- 14925946402938
-- 3706820676200


main = do
  input <- readFile "day14.input"
  print . day14_1 . lines $ input
  print . day14_2 . lines $ input