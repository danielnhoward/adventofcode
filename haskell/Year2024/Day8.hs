module Year2024.Day8 where

import Utils.List (pairs, withIndex, withCoordinatesFromInput)
import Utils.Run (run)

import Data.List (nub)

type Coordinate = (Int, Int)
data Grid = Grid [(Char, [Coordinate])] Coordinate deriving Show

partA :: String -> Int
partA = length . nub . coords . antinodeGrid antinodesForPair . procInput

procInput :: String -> Grid
procInput input = Grid charList (maximum (map fst input'))
  where
    input' = withCoordinatesFromInput input
    charRange = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
    charList = [(x, charCoords x input') | x <- charRange]

charCoords :: Char -> [(Coordinate, Char)] -> [Coordinate]
charCoords x = map fst . filter ((== x) . snd)

antinodeGrid :: AntinodeProcessor -> Grid -> Grid
antinodeGrid f (Grid xs bounds) = Grid (map procNode xs) bounds
  where procNode (x, coords) = (x, antinodes f coords bounds)

coords :: Grid -> [Coordinate]
coords (Grid xs _) = concatMap snd xs

antinodes :: AntinodeProcessor -> [Coordinate] -> Coordinate -> [Coordinate]
antinodes f coords bounds
  = concatMap (f bounds) (pairs coords)

type AntinodeProcessor = Coordinate -> (Coordinate, Coordinate) -> [Coordinate]

antinodesForPair :: AntinodeProcessor
antinodesForPair bounds ((x1, y1), (x2, y2))
  = filter (testBounds bounds) [(x1 - x', y1 - y'), (x2 + x', y2 + y')]
  where
    x' = x2 - x1
    y' = y2 - y1

testBounds :: Coordinate -> Coordinate -> Bool
testBounds (maxX, maxY)  (x, y)= x >= 0 && y >= 0 && x <= maxX && y <= maxY

partB :: String -> Int
partB = length . nub . coords . antinodeGrid antinodesForPairLine . procInput

antinodesForPairLine :: AntinodeProcessor
antinodesForPairLine bounds (coord1@(x1, y1), coord2@(x2, y2))
  = filterBounds coords1 ++ filterBounds coords2
  where
    x' = x2 - x1
    y' = y2 - y1
    runOp f (x, y) = (x `f` x', y `f` y')
    coords1 = iterate (runOp (-)) coord1
    coords2 = iterate (runOp (+)) coord2
    filterBounds = takeWhile (testBounds bounds)

main :: IO ()
main = run partA partB 2024 8
