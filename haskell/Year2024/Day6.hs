module Year2024.Day6 where

import Utils.List (withIndex)
import Utils.Run (run)

import Data.List (nub)
import Data.Set (Set, empty, member, insert, size, difference, singleton, elems)
import Data.Array (Array, array, assocs, (!), bounds)
import Data.Maybe (fromJust)

data Cell = Empty | Obstruction | Guard deriving (Eq, Show)
type Coordinate = (Int, Int)
type Grid = Array Coordinate Cell
type State = (Coordinate, Char)

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

getGuardCoordinates :: Grid -> Coordinate
getGuardCoordinates grid = head [i | (i, e) <- assocs grid, e == Guard]

direction :: Char -> Coordinate
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
partA input = size (moves grid (guardPos, guard))
  where
    grid = procInput input
    guardPos@(x, y) = getGuardCoordinates grid
    index = x + y * (2 + fst (snd (bounds grid)))
    guard = input !! index

walk :: Grid -> State -> Maybe State
walk = walkNewObstructions []

walkNewObstructions :: [Coordinate] -> Grid -> State -> Maybe State
walkNewObstructions newObstructions grid (pos@(x, y), guard)
  | x' < 0 || y' < 0 || x' > maxX || y' > maxY = Nothing
  | grid ! newPos == Obstruction || newPos `elem` newObstructions
    = Just (pos, turnRight guard)
  | otherwise = Just (newPos, guard)
  where
    (maxX, maxY) = snd (bounds grid)
    (dirX, dirY) = direction guard
    newPos@(x', y') = (x + dirX, y + dirY)

moves :: Grid -> State -> Set Coordinate
moves grid firstState = go firstState empty
  where
    go :: State -> Set Coordinate -> Set Coordinate
    go state@(pos@(x, y), guard) history = case walk grid state of
      Just newState -> go newState history'
      Nothing -> history'
      where history' = pos `insert` history

partB :: String -> Int
partB input = length (filter (loops grid (guardPos, guard)) (elems coordsList))
  where
    grid = procInput input
    guardPos@(x, y) = getGuardCoordinates grid
    index = x + y * (2 + fst (snd (bounds grid)))
    guard = input !! index
    coordsList = difference (moves grid (guardPos, guard)) (singleton guardPos)

loops :: Grid -> State -> Coordinate -> Bool
loops grid startState newObstruction = case walk' grid startState of
  Just nextState -> go startState nextState 1 1
  Nothing -> False
  where
    walk' = walkNewObstructions [newObstruction]
    go :: State -> State -> Int -> Int -> Bool
    go slowState fastState power length
      | slowState == fastState = True
      | otherwise = case walk' grid fastState of
        Just newState -> if power == length
          then go fastState newState (power * 2) 1
          else go slowState newState power (length + 1)
        Nothing -> False

main :: IO ()
main = run partA partB 2024 6
