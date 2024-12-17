module Utils.Grid (
  Coordinate,
  Grid,
  procGrid,
  directions,
  inBound,
) where

import Utils.List (withCoordinatesFromInput)

import Data.Array (Array, array, bounds)

type Coordinate = (Int, Int)
type Grid a = Array Coordinate a

procGrid :: (Char -> a) -> String -> Grid a
procGrid conv input = array ((0, 0), maximum (map fst coords)) (map convert coords)
  where
    coords = withCoordinatesFromInput input
    convert (pos, x) = (pos, conv x)

directions :: Grid a -> Coordinate -> [Coordinate]
directions grid (x, y) = filter (inBound (bounds grid)) [left, right, up, down]
  where
    left = (x - 1, y)
    right = (x + 1, y)
    up = (x, y - 1)
    down = (x, y + 1)

inBound :: (Coordinate, Coordinate) -> Coordinate -> Bool
inBound ((minX, minY), (maxX, maxY)) (x, y)
  = x >= minX && y >= minY && x <= maxX && y <= maxY
