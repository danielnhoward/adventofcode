module Year2024.Day6 where

import Utils.List (getIndex)
import Utils.Run (run)

import Data.List (nub)
import Data.Set (Set, empty, member, insert)

data Cell = Empty | Obstruction | Guard deriving (Eq, Show)

partA :: String -> Int
partA = nMoves . runMovesVariation moves

procInput :: String -> [[Cell]]
procInput = map (map procCell) . lines

procCell :: Char -> Cell
procCell '.' = Empty
procCell '#' = Obstruction
procCell '^' = Guard
procCell '>' = Guard
procCell 'v' = Guard
procCell '<' = Guard

getGuardCoordinates :: [[Cell]] -> (Int, Int)
getGuardCoordinates grid = (x, y)
  where
    y = getIndex (elem Guard) grid
    x = getIndex (== Guard) (grid !! y)

direction :: Char -> (Int, Int)
direction '^' = (0, -1)
direction '>' = (1, 0)
direction 'v' = (0, 1)
direction '<' = (-1, 0)

getCell :: [[Cell]] -> (Int, Int) -> Cell
getCell grid (x, y) = grid !! y !! x

turnRight :: Char -> Char
turnRight '^' = '>'
turnRight '>' = 'v'
turnRight 'v' = '<'
turnRight '<' = '^'

moves :: [[Cell]] -> (Int, Int) -> Char -> [(Int, Int)]
moves grid pos@(x, y) guard
  | x < 0 || y < 0 || x >= maxX || y >= maxY = []
  | x' < 0 || y' < 0 || x' >= maxX || y' >= maxY = [(x, y)]
  | newGuardCell == Obstruction = moves grid pos (turnRight guard)
  | otherwise = pos : moves grid newPos guard
  where
    maxX = length (head grid)
    maxY = length grid
    (dirX, dirY) = direction guard
    newPos@(x', y') = (x + dirX, y + dirY)
    newGuardCell = getCell grid newPos

runMovesVariation :: ([[Cell]] -> (Int, Int) -> Char -> a) -> String ->  a
runMovesVariation f input = f grid (x, y) (input !! index)
  where
    grid = procInput input
    (x, y) = getGuardCoordinates grid
    index = x + y * (1 + length (head (lines input)))

nMoves :: [(Int, Int)] -> Int
nMoves out = length (nub  out)

partB :: String -> Int
partB = length . filter (runMovesVariation loops) . getVariations

getVariations :: String -> [String]
getVariations [] = []
getVariations ('.' : xs) = ('#' : xs) : map ('.' :) (getVariations xs)
getVariations (x : xs) = map (x :) (getVariations xs)

obstructionsSet :: [[Cell]] -> Set (Int, Int)
obstructionsSet grid = go grid 0 0
  where
    go :: [[Cell]] -> Int -> Int -> Set (Int, Int)
    go [] _ _ = empty
    go ([] : xss) _ y = go xss 0 (y + 1)
    go ((Obstruction : xs) : xss) x y = (x, y) `insert` go (xs : xss) (x + 1) y
    go ((_ : xs) : xss) x y = go (xs : xss) (x + 1) y

loops :: [[Cell]] -> (Int, Int) -> Char -> Bool
loops grid startPos startGuard = go startPos startGuard empty
  where
    maxX = length (head grid)
    maxY = length grid
    obstructions = obstructionsSet grid
    go :: (Int, Int) -> Char -> Set (Char, Int, Int) -> Bool
    go pos@(x, y) guard history
      | x < 0 || y < 0 || x >= maxX || y >= maxY = False
      | x' < 0 || y' < 0 || x' >= maxX || y' >= maxY = False
      | (guard, x, y) `member` history = True
      | newPos `member` obstructions = go pos (turnRight guard) ((guard, x, y) `insert` history)
      | otherwise = go newPos guard history
      where
        (dirX, dirY) = direction guard
        newPos@(x', y') = (x + dirX, y + dirY)

main :: IO ()
main = run partA partB 2024 6
