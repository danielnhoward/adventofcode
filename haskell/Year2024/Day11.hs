module Year2024.Day11 where

import Utils.Run (run)

import Data.Map (Map, empty, (!), member, insert)

partA :: String -> Int
partA = runBlink 25 . procInput

procInput :: String -> [Int]
procInput = map read . words

runBlink :: Int -> [Int] -> Int
runBlink n = sum . map (blink n)

blink :: Int -> Int -> Int
blink = go
  where
    go :: Int -> Int -> Int
    go 0 _ = 1
    go n 0 = go (n - 1) 1
    go n s
      | even len = go (n - 1) s1 + go (n - 1) s2
      | otherwise = go (n - 1) (s * 2024)
      where
        len = floor (logBase 10 (fromIntegral s)) + 1
        divisor = 10 ^ (len `div` 2)
        s1 = s `div` divisor
        s2 = s `mod` divisor
blink' :: Int -> [Int] -> Int
blink' = go 0 empty
  where
    go :: Int -> Map (Int, Int) Int -> Int -> [Int] -> Int
    go acc _ _ [] = acc
    go acc _ 0 stones = acc + length stones
    -- go acc cache n (0 : stones) = go acc empty (n - 1) [1] + go 0 cache n stones
    go acc cache n (0 : stones)
      | (n - 1, 1) `member` cache = cache ! (n, 0)
      | otherwise = s' + go 0 (insert (n - 1, 1) s' cache) n stones
      where s' = go acc empty (n - 1) [1]
    go acc cache n (s : stones)
      | lenEven && s1Cached && s2Cached = go acc empty n stones + cache ! (n, s1) + cache ! (n, s2)
      | lenEven && s1Cached = go acc (insert (n, s) s2' cache) n stones + cache ! (n, s1) + s2'
      | even len = go acc empty (n - 1) [s1, s2] + go 0 cache n stones
      | otherwise = go acc empty (n - 1) [s * 2024] + go 0 cache n stones
      where
        len = floor (logBase 10 (fromIntegral s)) + 1
        lenEven = even len
        divisor = 10 ^ (len `div` 2)
        s1 = s `div` divisor
        s2 = s `mod` divisor
        s2024 = s * 2024
        s1' = go 0 empty (n - 1) [s1]
        s2' = go 0 empty (n - 1) [s2]
        s2024' = go 0 empty (n - 1) [s2024]
        s1Cached = (n, s1) `member` cache
        s2Cached = (n, s2) `member` cache
        s2024Cached = (n, s2024) `member` cache

partB :: String -> Int
partB = runBlink 75 . procInput

main :: IO ()
main = run partA partB 2024 11
