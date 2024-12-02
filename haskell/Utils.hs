module Utils (runSingle, run) where


type PartF a = Show a => String -> a
type Year = Int
type Day = Int

runSingle :: Show a => PartF a -> Year -> Day -> IO ()
runSingle f year day = do
  input <- readFile (path "test")
  putStrLn $ "  Test: " ++ show (f input)
  input <- readFile (path "input")
  putStrLn $ "  Input: " ++ show (f input)
  where path s = "Year" ++ show year ++ "/input/" ++ show day ++ "/" ++ s ++ ".txt"

run :: (Show a, Show b) => PartF a -> PartF b -> Year -> Day -> IO ()
run fa fb year day = do
  putStrLn "Part A:"
  runSingle fa year day
  putStrLn "Part B:"
  runSingle fb year day
