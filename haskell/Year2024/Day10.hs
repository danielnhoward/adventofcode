module Year2024.Day10 where

import Utils.List (withCoordinatesFromInput)
import Utils.Run (run)

import Data.Array (Array, array, assocs, bounds, (!))
import Data.Char (digitToInt)
import Data.List (nub)

type Coordinate = (Int, Int)
type Grid = Array Coordinate Int

partA :: String -> Int
partA input = sum (map (length . nub . walk grid 0) heads)
  where
    grid = procInput input
    heads = trailheads grid

procInput :: String -> Grid
procInput input = array ((0, 0), maximum (map fst coords)) (map convert coords)
  where
    coords = withCoordinatesFromInput input
    convert (pos, c) = (pos, digitToInt c)

trailheads :: Grid -> [Coordinate]
trailheads = map fst . filter ((== 0) . snd) . assocs

walk :: Grid -> Int -> Coordinate -> [[(Coordinate, Int)]]
walk grid 9 pos = [[(pos, 9)]]
walk grid i pos = concatMap (walk grid i') validDirs
  where
    i' = i + 1
    dirs = directions grid pos
    validDirs = filter ((== i') . (grid !)) dirs

directions :: Grid -> Coordinate -> [Coordinate]
directions grid (x, y) = filter (inBound (0, 0) maxPos) [left, right, up, down]
  where
    maxPos = snd (bounds grid)
    left = (x - 1, y)
    right = (x + 1, y)
    up = (x, y - 1)
    down = (x, y + 1)

inBound :: Coordinate -> Coordinate -> Coordinate -> Bool
inBound (minX, minY) (maxX, maxY) (x, y)
  = x >= minX && y >= minY && x <= maxX && y <= maxY

partB :: String -> Int
partB input = sum (map (length . walk grid 0) heads)
  where
    grid = procInput input
    heads = trailheads grid

main :: IO ()
main = run partA partB 2024 10
