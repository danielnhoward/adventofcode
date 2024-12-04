module Year2024.Day3 where

import Utils.Run (run)

import Text.Regex.TDFA ((=~))

partA :: String -> Int
partA = sum . map (uncurry (*)) . getMulText

mulFuncRegex :: String
mulFuncRegex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

getMulText :: String -> [(Int, Int)]
getMulText s
  | rhs == "" = []
  | otherwise = (read n1, read n2) : getMulText rhs
  where
    (_ :: String, _ :: String, rhs, ns) = s =~ mulFuncRegex
    [n1, n2] = ns

partB :: String -> Int
partB = partA . getValidParts

doRegex :: String
doRegex = "do\\(\\)"

dontRegex :: String
dontRegex = "don't\\(\\)"

getValidParts :: String -> String
getValidParts "" = ""
getValidParts s = lhs ++ getValidPartsInverse rhs
  where (lhs, _ :: String, rhs) = s =~ dontRegex

getValidPartsInverse :: String -> String
getValidPartsInverse "" = ""
getValidPartsInverse s = getValidParts rhs
  where (_, _, rhs) = s =~ doRegex :: (String, String, String)

main :: IO ()
main = run partA partB 2024 3
