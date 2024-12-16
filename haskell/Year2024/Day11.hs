module Year2024.Day11 where

import Utils.Run (run)

import Data.List (nub)
import Data.Map (Map, fromList, elems, insertWith, foldrWithKey, empty)

partA :: String -> Int
partA = blink 25 . procInput

procInput :: String -> Map Int Int
procInput = fromList . map ((, 1) . read) . words

blink :: Int -> Map Int Int -> Int
blink 0 = sum . elems
blink n = blink (n - 1) . foldrWithKey blinkValue empty
blink' :: Int -> Map Int Int -> Map Int Int
blink' 0 = id
blink' n = blink' (n - 1) . foldrWithKey blinkValue empty

blinkValue :: Int -> Int -> Map Int Int -> Map Int Int
blinkValue 0 n = insertWith (+) 1 n
blinkValue x n
  | even len = insertWith (+) x1 n . insertWith (+) x2 n
  | otherwise = insertWith (+) (x * 2024) n
  where (len, x1, x2) = splitValue x

splitValue :: Int -> (Int, Int, Int)
splitValue x = (len, x1, x2)
  where
    len = floor (logBase 10 (fromIntegral x)) + 1
    divisor = 10 ^ (len `div` 2)
    x1 = x `div` divisor
    x2 = x `mod` divisor

partB :: String -> Int
partB = blink 75 . procInput

main :: IO ()
main = run partA partB 2024 11
