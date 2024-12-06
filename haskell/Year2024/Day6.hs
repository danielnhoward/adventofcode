module Year2024.Day6 where

import Utils.List (getIndex)
import Utils.Run (run)

import Data.List (nub)
import Data.Set (Set, empty, member, insert)
import Data.Array (Array, array, assocs, (!), bounds)

data Cell = Empty | Obstruction | Guard deriving (Eq, Show)
type Grid = Array (Int, Int) Cell

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

procInput :: String -> Grid
procInput s = array ((0,0), (length (head ls) - 1, length ls - 1))
  [((x, y), procCell c) | (y, row) <- withIndex ls, (x, c) <- withIndex row]
  where
    ls = lines s

procCell :: Char -> Cell
procCell '.' = Empty
procCell '#' = Obstruction
procCell '^' = Guard
procCell '>' = Guard
procCell 'v' = Guard
procCell '<' = Guard

getGuardCoordinates :: Grid -> (Int, Int)
getGuardCoordinates grid = head [i | (i, e) <- assocs grid, e == Guard]

direction :: Char -> (Int, Int)
direction '^' = (0, -1)
direction '>' = (1, 0)
direction 'v' = (0, 1)
direction '<' = (-1, 0)

turnRight :: Char -> Char
turnRight '^' = '>'
turnRight '>' = 'v'
turnRight 'v' = '<'
turnRight '<' = '^'

partA :: String -> Int
partA input = length (nub (moves grid guardPos guard))
  where
    grid = procInput input
    guardPos@(x, y) = getGuardCoordinates grid
    index = x + y * (2 + fst (snd (bounds grid)))
    guard = input !! index

moves :: Grid -> (Int, Int) -> Char -> [(Int, Int)]
moves grid pos@(x, y) guard
  | x < 0 || y < 0 || x > maxX || y > maxY = []
  | x' < 0 || y' < 0 || x' > maxX || y' > maxY = [(x, y)]
  | grid ! newPos == Obstruction = moves grid pos (turnRight guard)
  | otherwise = pos : moves grid newPos guard
  where
    (maxX, maxY) = snd (bounds grid)
    (dirX, dirY) = direction guard
    newPos@(x', y') = (x + dirX, y + dirY)

partB :: String -> Int
partB input = length (filter (loops grid guardPos guard) coordinatesList)
  where
    grid = procInput input
    guardPos@(x, y) = getGuardCoordinates grid
    index = x + y * (2 + fst (snd (bounds grid)))
    guard = input !! index
    (_ : coordinatesList) = nub (moves grid guardPos guard)

loops :: Grid -> (Int, Int) -> Char -> (Int, Int) -> Bool
loops grid startPos startGuard newObstruction = go startPos startGuard empty
  where
    (maxX, maxY) = snd (bounds grid)
    go :: (Int, Int) -> Char -> Set (Char, Int, Int) -> Bool
    go pos@(x, y) guard history
      | x < 0 || y < 0 || x > maxX || y > maxY = False
      | x' < 0 || y' < 0 || x' > maxX || y' > maxY = False
      | (guard, x, y) `member` history = True
      | grid ! newPos == Obstruction || newPos == newObstruction
        = go pos (turnRight guard) ((guard, x, y) `insert` history)
      | otherwise = go newPos guard history
      where
        (dirX, dirY) = direction guard
        newPos@(x', y') = (x + dirX, y + dirY)

main :: IO ()
main = run partA partB 2024 6
