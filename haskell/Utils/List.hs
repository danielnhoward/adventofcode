module Utils.List (compareList, isDecreasing, isIncreasing, subLists) where

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
