module Day20 where

import AoCHelper (split', iter, bin2Int)

import Data.List (elemIndex, lookup, transpose, union, intersect, (\\))
import Data.Map (fromList, toList, lookup, insert, Map)

inputToMap :: [String] -> (Int, [String])
inputToMap (caption : picture) = (nr, picture)
  where nr = read . last . words . init $ caption

toBorder :: (Int, [String]) -> (Int, [Int])
toBorder (nr, picture) = (nr, intLst)
  where intLst = map (bin2Int . map (== '#')) (lst ++ map reverse lst)
        lst = [top, right, left, bottom]
        top    = head picture
        right  = map last picture
        left   = map head picture
        bottom = last picture

howOften :: Int -> [Int] -> Int
howOften n = length . filter (== n)

countIn :: [Int] -> (Int, [Int]) -> (Int, Int)
countIn lst (nr, brd) = (nr, (sum . map (`howOften` lst) $ brd) - 8)

searchNext corner = filter ((== corner) . fst)

getRow border connections row = 
    (intersect border (map snd . searchNext (head row) $ connections) \\ row) ++ row
    
nextRow (row, connections) = (xs, ys)
  where Just xs = mapM (`Data.List.lookup` ys) row
        ys = filter (not . (`elem` row) . snd ) connections

topToOp = [reverse, transpose, reverse . transpose, id, 
           reverse . map reverse, map reverse . transpose, reverse . transpose . reverse, map reverse]
           
leftToOp = [map reverse . transpose, id, map reverse, transpose,
            reverse . transpose . reverse, reverse, reverse . map reverse, reverse . transpose]

goTo :: [[String] -> [String]] -> Int -> 
        (Map Int [Int], Map Int [String]) -> (Int, Int) -> (Map Int [Int], Map Int [String])
goTo operations side (borderMap, pictureMap) (start, next) = (newBorderMap, newPictureMap)
  where newPictureMap = insert next newPicture pictureMap
        newBorderMap = insert next newBorder borderMap
        (_, newBorder) = toBorder (next, newPicture)
        newPicture = (operations !! index) picture
        Just index = elemIndex (startBorders !! side) nextBorders
        Just picture = Data.Map.lookup next pictureMap
        Just nextBorders = Data.Map.lookup next borderMap
        Just startBorders = Data.Map.lookup start borderMap

goingUp :: (Map Int [Int], Map Int [String]) -> [Int] -> (Map Int [Int], Map Int [String])
goingUp (borderMap, pictureMap) row = foldl (goTo topToOp 0) (borderMap, pictureMap) pairs
  where pairs = zip row (tail row)

goingLeft :: (Map Int [Int], Map Int [String]) -> [Int] -> (Map Int [Int], Map Int [String])
goingLeft (borderMap, pictureMap) col = foldl (goTo leftToOp 2) (borderMap, pictureMap) pairs
  where pairs = zip col (tail col)

findPicture :: Map Int [String] -> Int -> [String]
findPicture picMap n = map (init . tail) . init . tail $ pic
  where Just pic = Data.Map.lookup n picMap

toBool :: [String] -> [Bool]
toBool = concatMap (map (== '#')) 

isMonster :: [String] -> Bool
isMonster xs = test == monsterBool
  where test = zipWith (&&) monsterBool (toBool xs)
        monsterBool = toBool monster

monster = ["..................#.", 
           "#....##....##....###", 
           ".#..#..#..#..#..#..."]

checkColumn :: [String] -> Int
checkColumn = length . filter id . map (isMonster . take 3) . take 94 . iterate tail. map (take 20)

hashCount :: [String] -> Int
hashCount = sum . map (length . filter (== '#'))

day20_1 lst = product corners
  where corners = map fst . filter ((== 4) . snd) $ counts
        counts = map (countIn borders) intLst
        borders = concatMap snd intLst
        intLst = map (toBorder . inputToMap) . split' null $ lst

day20_2 lst = hashCount bigPicture - hashCount monster * monsterCount
  where monsterCount = sum . map checkColumn . take 77 . iterate (map tail) . reverse . transpose $ bigPicture
        bigPicture = concatMap (foldr1 (zipWith (++)) . map (findPicture finalPictures)) finalGrid
        finalGrid = transpose . reverse . map reverse $ grid
        (_, finalPictures) = foldl goingUp rightToLeft grid
        rightToLeft = goingLeft (borderMap, pictureMap) (map head grid)
        borderMap = fromList intLst
        pictureMap = fromList inputMap
        grid = take 12 . map fst . iterate nextRow $ (firstRow, connections)
        firstRow = iter (getRow border connections) 10 [next, startCorner]
        [(_, next), (_, _)] = searchNext startCorner connections 
        connections = [(x, y) | (x, b1) <- intLst, (y, b2) <- intLst, x /= y, 2 == length (b1 `intersect` b2)]
        startCorner = head corners
        border = sides `union` corners
        sides = map fst . filter ((== 6) . snd) $ counts
        corners = map fst . filter ((== 4) . snd) $ counts
        counts = map (countIn borders) intLst
        borders = concatMap snd intLst
        intLst = map toBorder inputMap
        inputMap = map inputToMap . split' null $ lst


main = do
  input <- readFile "day20.input"
  print . day20_1 $ lines input
  print . day20_2 $ lines input