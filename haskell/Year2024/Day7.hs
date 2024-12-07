module Year2024.Day7 where

import Utils.Run (run)

import Data.List.Split (splitOn)

partA :: String -> Int
partA = sum . map fst . filter (validCalculation opsList) . procInput
  where opsList = [(+), (*)]

procInput :: String -> [(Int, [Int])]
procInput = map procLine . lines

procLine :: String -> (Int, [Int])
procLine line = (read target, map read (words values))
  where [target, values] = splitOn ":" line

opsOrder :: [Int -> Int -> Int] -> Int -> [[Int -> Int -> Int]]
opsOrder _ 1 = []
opsOrder ops 2 = map pure ops
opsOrder ops n = concatMap (\ops' -> map (: ops') ops) (opsOrder ops (n - 1))

applyOps :: [Int] -> [Int -> Int -> Int] -> Int
applyOps [x] [] = x
applyOps (x1 : x2 : xs) (f : fs) = applyOps (f x1 x2 : xs) fs

validCalculation :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
validCalculation ops (target, xs) = target `elem` values
  where values = map (applyOps xs) (opsOrder ops (length xs))

partB :: String -> Int
partB = sum . map fst . filter (validCalculation opsList) . procInput
  where opsList = [(+), (*), concatValues]

concatValues :: Int -> Int -> Int
concatValues x1 x2 = read (show x1 ++ show x2)

main :: IO ()
main = run partA partB 2024 7
