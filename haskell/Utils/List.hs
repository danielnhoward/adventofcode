module Utils.List (
  compareList,
  isDecreasing,
  isIncreasing,
  subLists,
  subListsLength,
  pairs,
  getVertical,
  getDiagonals,
  removeFirst,
  insertValue,
  getIndex,
  withIndex,
  withCoordinates,
  withCoordinatesFromInput,
) where

import Data.List (transpose)
import Data.Tuple (swap)

compareList :: (Int -> Int -> Bool) -> [Int] -> Bool
compareList _ [] = True
compareList _ [_] = True
compareList f (x1 : x2 : xs) = x1 `f` x2 && compareList f (x2 : xs)

isDecreasing :: [Int] -> Bool
isDecreasing = compareList (>)

isIncreasing :: [Int] -> Bool
isIncreasing = compareList (<)

subLists :: [a] -> [[a]]
subLists [] = []
subLists [x] = [[x], []]
subLists (x : xs) = map (x :) xs' ++ xs'
  where xs' = subLists xs

subListsLength :: Int -> [a] -> [[a]]
subListsLength l xs = filter ((l ==) . length) (subLists xs)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

getVertical :: [[a]] -> [[a]]
getVertical = transpose

getDiagonals :: [[a]] -> [[a]]
getDiagonals xs = getDiagonals' xs ++ getDiagonals' (map reverse xs)

getDiagonals' :: [[a]] -> [[a]]
getDiagonals' [] = []
getDiagonals' [xs] = map (: []) xs
getDiagonals' (xs : xss)
  = zipWith (:) xs ([] : diags) ++ drop (length xs - 1) diags
  where diags = getDiagonals' xss

removeFirst :: Eq a => [a] -> a -> [a]
removeFirst xs x = prefix ++ drop 1 suffix
  where (prefix, suffix) = break (== x) xs

insertValue :: [a] -> Int -> a -> [a]
insertValue xs 0 y = y : xs
insertValue (x : xs) n y = x : insertValue xs (n - 1) y

getIndex :: (a -> Bool) -> [a] -> Int
getIndex f = go 0
  where
    go _ [] = -1
    go n (x : xs)
      | f x = n
      | otherwise = go (n + 1) xs

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

withCoordinates :: Int -> [a] -> [((Int, Int), a)]
withCoordinates rowLength = map procValue . withIndex
  where procValue (i, x) = (swap (quotRem i rowLength), x)

withCoordinatesFromInput :: String -> [((Int, Int), Char)]
withCoordinatesFromInput input = withCoordinates rowLength strippedInput
  where
    rowLength = length (head (lines input))
    strippedInput = [x | x <- input, x /= '\n']
