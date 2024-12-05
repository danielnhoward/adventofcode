module Year2024.Day5 where

import Data.List.Split (splitOn)
import Utils.Read (readInt)
import Utils.List (pairs, removeFirst, insertValue)
import Utils.Run (run)

partA :: String -> Int
partA = sum . map middleValue . getOrderedRules

procInput :: String -> ([(Int, Int)], [[Int]])
procInput input = (map procRule (lines rules), map procOrder (lines orders))
  where [rules, orders] = splitOn "\n\n" input

procRule :: String -> (Int, Int)
procRule rule = (readInt lhs, readInt rhs)
  where [lhs, rhs] = splitOn "|" rule

procOrder :: String -> [Int]
procOrder = map readInt . splitOn ","

isOrderValid :: [(Int, Int)] -> [Int] -> Bool
isOrderValid rules = not . any (\(x, y) -> (y, x) `elem` rules) . pairs

getOrderedRules :: String -> [[Int]]
getOrderedRules input = filter (isOrderValid rules) orders
  where (rules, orders) = procInput input

middleValue :: Integral a => [a] -> a
middleValue xs = xs !! (length xs `div` 2)

partB :: String -> Int
partB input = sum (map (middleValue . order rules) (getUnorderedRules input))
  where (rules, orders) = procInput input

getUnorderedRules :: String -> [[Int]]
getUnorderedRules input = filter (not . isOrderValid rules) orders
  where (rules, orders) = procInput input

order :: [(Int, Int)] -> [Int] -> [Int]
order rules xs = go xs []
  where
    go :: [Int] -> [Int] -> [Int]
    go [] ordVs = ordVs
    go (a : as) ordVs = go as (head [insert i | i <- range, isValid i])
      where
        range = [0..(length ordVs)]
        insert i = insertValue ordVs i a
        isValid i = isOrderValid rules (insert i)

main :: IO ()
main = run partA partB 2024 5
