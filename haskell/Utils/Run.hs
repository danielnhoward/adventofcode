module Utils.Run (run, runA, runB) where

import System.Directory ( doesFileExist )

data Part = A | B deriving Show

type PartF a = Show a => String -> a
type Year = Int
type Day = Int

runSingle :: Show a => String -> PartF a -> String -> IO ()
runSingle text f path = readFile path >>= putStrLn . getText
  where getText input = "  " ++ text ++ ": " ++ show (f input)

runPart :: Show a => Part -> PartF a -> Year -> Day -> IO ()
runPart part f year day = do
  runTests f (path "test") (path ("test" ++ show part))
  runSingle "Main Input" f (path "input")
  where
    path s = "Year" ++ show year ++ "/input/" ++ show day ++ "/" ++ s ++ ".txt"

runTests :: Show a => PartF a -> String -> String -> IO ()
runTests f testPath testPartPath
  = doesFileExist testPath >>= runSingle "Test Input" f . path
  where path testExists = if testExists then testPath else testPartPath

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
