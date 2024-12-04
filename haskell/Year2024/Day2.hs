module Year2024.Day2 where

import Utils.List (compareList, isDecreasing, isIncreasing, subLists)
import Utils.Run (run)
import Utils.Read (readInt)

procInput :: String -> [[Int]]
procInput = map (map readInt . words) . lines

partA :: String -> Int
partA = length . filter reportSafe . procInput

reportSafe :: [Int] -> Bool
reportSafe xs =
  (isDecreasing xs || isIncreasing xs) && compareList compareDiffs xs

compareDiffs :: Int -> Int -> Bool
compareDiffs x y = diff <= 3 && diff >= 1
  where diff = abs (x - y)

partB :: String -> Int
partB = length . filter (any reportSafe . getSubLists) . procInput

getSubLists :: [Int] -> [[Int]]
getSubLists xs = filter ((length xs - 1 <=) . length) (subLists xs)

main :: IO ()
main = run partA partB 2024 2
