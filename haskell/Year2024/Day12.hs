module Year2024.Day12 where

import Utils.Grid (Coordinate, Grid, procGrid, directions)
import Utils.List (withCoordinatesFromInput)

import Data.Array (Array, (!), elems)
import Data.Set (Set, empty, member, insert)
import Data.List (nub)

procInput :: String -> (Grid Char, [Char])
procInput input = (procGrid id input, nub (concat (lines input)))

getCount :: Char -> Grid Char -> Int
getCount c = length . filter (== c) . elems

getPerimeter :: Grid Char -> Char -> Coordinate -> Int
getPerimeter grid c = (`div` 2) . go empty
  where
    go :: Set Coordinate -> Coordinate -> Int
    go visited pos
      | null unexplCells = 0
      | grid ! pos /= c = next
      | otherwise = length opCells + next
      where
        dirs = directions grid pos
        opCells = filter ((/= c) . (grid !)) dirs
        unexplCells = filter (not . (`member` visited)) dirs
        next = go (pos `insert` visited) (head unexplCells)
