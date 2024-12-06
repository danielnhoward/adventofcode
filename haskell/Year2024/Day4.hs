module Year2024.Day4 where

import Utils.List (getVertical, getDiagonals)
import Utils.Run (run)

import Data.Char (toLower)

partA :: String -> Int
partA = sum . map countXmas . getParts . procInput

procInput :: String -> [String]
procInput = lines . map toLower

getParts :: [String] -> [String]
getParts xs = all ++ map reverse all
  where
    vertical = getVertical xs
    diagonals = getDiagonals xs
    all = xs ++ vertical ++ diagonals

countXmas :: String -> Int
countXmas [] = 0
countXmas ('x' : 'm' : 'a' : 's' : xs) = 1 + countXmas xs
countXmas (_ : xs) = countXmas xs

partB :: String -> Int
partB = length . filter matchMas . getBoxes . procInput

getAllDiagonals :: [String] -> [String]
getAllDiagonals xs = diagonals ++ map reverse diagonals
  where diagonals = getDiagonals xs

matchMas :: [String] -> Bool
matchMas = (== 2) . length . filter (== "mas") . getAllDiagonals

getBoxes :: [String] -> [[String]]
getBoxes xs
  = [getBox xs i j | i <- [0..(length xs - 3)], j <- [0..(length xs - 3)]]

getBox :: [String] -> Int -> Int -> [String]
getBox xs i j
  = [ [xs !! i !! j, xs !! i !! (j + 1), xs !! i !! (j + 2)]
    , [xs !! (i + 1) !! j, xs !! (i + 1) !! (j + 1), xs !! (i + 1) !! (j + 2)]
    , [xs !! (i + 2) !! j, xs !! (i + 2) !! (j + 1), xs !! (i + 2) !! (j + 2)]
    ]

main :: IO ()
main = run partA partB 2024 4
