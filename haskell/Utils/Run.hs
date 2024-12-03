module Utils.Run (run, runA, runB) where

import System.Directory ( doesFileExist )

data Part = A | B deriving Show

type PartF a = Show a => String -> a
type Year = Int
type Day = Int

runPart :: Show a => Part -> PartF a -> Year -> Day -> IO ()
runPart part f year day = do
  runTests f (path "test") (path ("test" ++ show part))
  input <- readFile (path "input")
  putStrLn $ "  Input: " ++ show (f input)
  where
    path s = "Year" ++ show year ++ "/input/" ++ show day ++ "/" ++ s ++ ".txt"

runTests :: Show a => PartF a -> String -> String -> IO ()
runTests f testPath testPartPath = doesFileExist testPath >>= (\exists ->
  if exists then do
    input <- readFile testPath
    putStrLn $ "  Test Input: " ++ show (f input)
  else do
    input <- readFile testPartPath
    putStrLn $ "  Test Input: " ++ show (f input)
  )

runA :: Show a => PartF a -> Year -> Day -> IO ()
runA = runPart A

runB :: Show a => PartF a -> Year -> Day -> IO ()
runB = runPart B

run :: (Show a, Show b) => PartF a -> PartF b -> Year -> Day -> IO ()
run fa fb year day = do
  putStrLn "Part A:"
  runA fa year day
  putStrLn "Part B:"
  runB fb year day
