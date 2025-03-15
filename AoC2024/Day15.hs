module Day15 where

import AoCHelper (Pair, findChar, splitBy, (@), (.+.), (.-.))
import Data.List (delete, (\\), sort, intersect)

day15_1 :: [String] -> Int
day15_1 input = calculate . foldl move (robo, boxes) $ concat moves
  where
    calculate = sum . map ((+) <$> (100 *) . fst <*> snd) . snd
    robo = head $ findChar grid '@'
    boxes = findChar grid 'O'
    [grid, moves] = splitBy (== "") input

    move (robo, boxes) c
      | grid @ nextPos == '#' = (robo, boxes)
      | nextPos `elem` boxes =
        let (moveable, boxes') = canMoveBox nextPos boxes c
         in if moveable then (nextPos, boxes') else (robo, boxes)
      | otherwise = (nextPos, boxes)
      where nextPos = next c robo

    canMoveBox box boxes c
      | grid @ nextPos == '#' = (False, boxes)
      | nextPos `elem` boxes =
        let (moveable, boxes') = canMoveBox nextPos boxes c
         in (moveable, if moveable then moveBox boxes' else boxes)
      | otherwise = (True, moveBox boxes)
      where
        nextPos = next c box
        moveBox = (nextPos :) . delete box


data Box = Box { left :: Pair Int, right :: Pair Int } deriving (Show, Eq)

createBoxLeft :: Pair Int -> Box
createBoxLeft left = Box left (left .+. (0, 1))

createBoxRight :: Pair Int -> Box
createBoxRight right = Box (right .-. (0, 1)) right

day15_2 input = foldl move (robo, boxes) "<vv>^<v^>v>^vv^v>v<>v" 
  where
    boxes = zipWith Box lefts rights
    robo = head $ findChar newGrid '@'
    lefts = findChar newGrid '['
    rights = map (.+. (0, 1)) lefts
    newGrid = map exchange grid
    [grid, moves] = splitBy (== "") input

    move (robo, boxes) c
      | newGrid @ nextPos == '#' = (robo, boxes)
      | createBoxLeft nextPos `elem` boxes =
        let (moveable, boxes') = canMoveBox (createBoxLeft nextPos) boxes c
         in if moveable then (nextPos, boxes') else (robo, boxes)
      | createBoxRight nextPos `elem` boxes =
        let (moveable, boxes') = canMoveBox (createBoxRight nextPos) boxes c
         in if moveable then (nextPos, boxes') else (robo, boxes)
      | otherwise = (nextPos, boxes)
      where nextPos = next c robo

    canMoveBox box boxes c
      | any ((== '#') . (newGrid @)) nextPos = (False, boxes)
      | c == '<' && createBoxRight (head nextPos) `elem` boxes =
        let (moveable, boxes') = canMoveBox (createBoxRight (head nextPos)) boxes c
         in (moveable, if moveable then moveBox boxes' else boxes)
      | c == '>' && createBoxLeft (head nextPos) `elem` boxes =
        let (moveable, boxes') = canMoveBox (createBoxRight (head nextPos)) boxes c
         in (moveable, if moveable then moveBox boxes' else boxes)
      | c `elem` "^v" = undefined -- ToDo!!
      | otherwise = (True, moveBox boxes)
      where
        nextPos = nextPos' c box
        moveBox = ((if c == '<' then createBoxLeft else createBoxRight) (head nextPos) :) . delete box


exchange :: String -> String
exchange "" = ""
exchange ('#' : xs) = "##" ++ exchange xs
exchange ('O' : xs) = "[]" ++ exchange xs
exchange ('.' : xs) = ".." ++ exchange xs
exchange ('@' : xs) = "@." ++ exchange xs

nextPos' :: Char -> Box -> [Pair Int]
nextPos' '<' box = [next '<' $ left box]
nextPos' '>' box = [next '>' $ right box]
nextPos' c box = [next c $ left box, next c $ right box]

next :: Char -> Pair Int -> Pair Int
next '>' (x, y) = (x, y+1)
next '<' (x, y) = (x, y-1)
next '^' (x, y) = (x-1, y)
next 'v' (x, y) = (x+1, y)

--1465523

main :: IO ()
main = do
    input <- lines <$> readFile "day15exp.input"
    print $ day15_1 input
    print $ day15_2 input
