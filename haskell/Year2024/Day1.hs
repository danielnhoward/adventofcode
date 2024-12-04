module Year2024.Day1 where

import Utils.Read (readInt)
import Utils.Run (run)

import Data.List (sort)
import Control.Functor.HT (mapFst)

procInput :: String -> ([Int], [Int])
procInput = unzip . map readLine . lines

partA :: String -> Int
partA = sum . map (abs . uncurry (-)) . uncurry zip . sortPair . procInput

readLine :: String -> (Int, Int)
readLine = (\[n1, n2] -> (n1, n2)) . map readInt . words

sortPair :: Ord a => ([a], [a]) -> ([a], [a])
sortPair (xs, ys) = (sort xs, sort ys)

partB :: String -> Int
partB input = sum (map (getSimilarityScore l2) l1)
  where (l1, l2) = procInput input

getSimilarityScore :: [Int] -> Int -> Int
getSimilarityScore xs x = x * (x `count` xs)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

main :: IO ()
main = run partA partB 2024 1
