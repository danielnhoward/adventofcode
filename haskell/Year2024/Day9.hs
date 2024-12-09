module Year2024.Day9 where

import Utils.Run (run)

import Data.Char (digitToInt)

data FileCell = Block Int | Empty deriving (Show, Eq)

partA :: String -> Int
partA = shiftedChecksum . procInput

procInput :: String -> [FileCell]
procInput = go True 0
  where
    go :: Bool -> Int -> String -> [FileCell]
    go _ _ [] = []
    go takeInput i (x : xs) = replicate n block ++ go (not takeInput) i' xs
      where
        n = digitToInt x
        block = if takeInput then Block i else Empty
        i' = if takeInput then i + 1 else i

shiftedChecksum :: [FileCell] -> Int
shiftedChecksum blocks = go (length fullBlocks) 0 blocks (reverse fullBlocks)
  where
    fullBlocks = filter (/= Empty) blocks
    go :: Int -> Int -> [FileCell] -> [FileCell] -> Int
    go 0 _ _ _ = 0
    go l i (Empty : bs) (Block lb : lbs) = lb * i + go (l - 1) (i + 1) bs lbs
    go l i (Block b : bs) lbs = b * i + go (l - 1) (i + 1) bs lbs

partB :: String -> Int
partB = checksum . defragment . procFileBlocks

data FileBlock = FullBlock Int Int | EmptyBlock Int deriving (Show, Eq)

size :: FileBlock -> Int
size (FullBlock _ n) = n
size (EmptyBlock n) = n

getFileId :: FileBlock -> Int
getFileId (FullBlock n _) = n
getFileId (EmptyBlock _) = -1

replaceEmpty :: Int -> FileBlock -> FileBlock
replaceEmpty _ block@(EmptyBlock _) = block
replaceEmpty fileId block@(FullBlock blockId n)
  | fileId == blockId = EmptyBlock n
  | otherwise = block

procFileBlocks :: String -> [FileBlock]
procFileBlocks = filter ((/= 0) . size) . go True 0
  where
    go :: Bool -> Int -> String -> [FileBlock]
    go _ _ [] = []
    go isEmpty i (x : xs) = block n : go (not isEmpty) i' xs
      where
        n = digitToInt x
        block = if isEmpty then FullBlock i else EmptyBlock
        i' = if isEmpty then i + 1 else i

isFull :: FileBlock -> Bool
isFull (FullBlock _ _) = True
isFull _ = False

defragment :: [FileBlock] -> [FileBlock]
defragment blocks = go blocks (reverse (filter isFull blocks))
  where
    go :: [FileBlock] -> [FileBlock] -> [FileBlock]
    go bs [] = bs
    go bs (lb : lbs) = go (insert bs lb) lbs

insert :: [FileBlock] -> FileBlock -> [FileBlock]
insert [] block = [block]
insert (emptyB@(EmptyBlock emptySize) : bs) block@(FullBlock fileId fileSize)
  | emptySize == fileSize = block : filtered
  | emptySize > fileSize = block : EmptyBlock (emptySize - fileSize) : filtered
  | otherwise = emptyB : insert bs block
  where filtered = map (replaceEmpty fileId) bs
insert allBs@(b@(FullBlock fileId _) : bs) block@(FullBlock blockFileId _)
  | fileId == blockFileId = allBs
  | otherwise = b : insert bs block

checksum :: [FileBlock] -> Int
checksum = go 0
  where
    go :: Int -> [FileBlock] -> Int
    go _ [] = 0
    go i (EmptyBlock n : bs) = go (i + n) bs
    go i (FullBlock fileId n : bs) = sum [j * fileId | j <- [i..(i+n)-1]] + go (i + n) bs

main :: IO ()
main = run partA partB 2024 9
