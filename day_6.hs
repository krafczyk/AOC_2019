import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import Data.List.Split
import Data.Typeable

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1),
                   ("debug_test_2", ["-d2"], "Number of steps to debug second task", 1) ]

type OrbitMap = Map.Map String [String]

countOrbits1 :: OrbitMap -> Int
countOrbits1 orbitMap = sum $ map (numDescendents orbitMap) orbiters
    where orbiters = Map.keys orbitMap

numDescendents :: OrbitMap -> String -> Int
numDescendents orbitMap node
    | children == [] = 0 -- No children
    | otherwise = length children + (sum vals)
    where children = getDescendents orbitMap node
          vals = map (numDescendents orbitMap) children

getDescendents :: OrbitMap -> String -> [String]
getDescendents orbitMap node = children
    where children = case Map.lookup node orbitMap of
                         Nothing -> []
                         Just x -> x

type DistMap = Map.Map String Int

shortestDistImp :: OrbitMap -> OrbitMap -> (DistMap, [String]) -> (DistMap, [String])
shortestDistImp child_map parent_map (visited,[]) = (visited, [])
shortestDistImp child_map parent_map (visited, queue) = (Map.insert next (shortest_dist+1) visited, new_queue ++ to_visit)
    where next = head queue
          new_queue = drop 1 queue
          children = getDescendents child_map next
          parents = getDescendents parent_map next
          to_visit = filter (\n -> not $ n `elem` new_queue) $ filter (\n -> Map.notMember n visited) $ children ++ parents -- Not visited nodes not in the queue already
          to_check = filter (\n -> Map.member n visited) $ children ++ parents -- We can check distances to nodes already visited here.
          shortest_dist = minimum $ map (\n -> visited Map.! n) to_check

shortestDist :: OrbitMap -> OrbitMap -> String -> String -> Int
shortestDist child_map parent_map start end = (fst $ head $ reverse state_gen) Map.! end
    where init_visited = Map.singleton start 0
          children = getDescendents child_map start
          parents = getDescendents parent_map start
          state_gen = U.takeWhileInclusive (\(visited, _) -> Map.notMember end visited) $ iterate (shortestDistImp child_map parent_map) (init_visited, children ++ parents)

shortestDistDebug num child_map parent_map start end = state_gen
    where init_visited = Map.singleton start 0
          children = getDescendents child_map start
          parents = getDescendents parent_map start
          state_gen = take num $ iterate (shortestDistImp child_map parent_map) (init_visited, children ++ parents)


fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let child_map = foldl1 (Map.unionWith (++)) $ map (\[x,y] -> Map.singleton y [x]) $ map (splitOn ")") $ lines file_data
                                parent_map = foldl1 (Map.unionWith (++)) $ map (\[x,y] -> Map.singleton x [y]) $ map (splitOn ")") $ lines file_data
                            putStrLn $ "Task 1: " ++ (show $ countOrbits1 child_map)
                            let init = sequence [Map.lookup "YOU" child_map, Map.lookup "SAN" child_map]
                            case init of
                                Nothing -> putStrLn "Can't solve part 2! Can't find either start or end points!"
                                Just [starts, ends] -> case Map.lookup "debug_test_2" argMap of
                                                           Nothing -> putStrLn $ "Task 2: " ++ (show $ shortestDist child_map parent_map (head starts) (head ends))
                                                           Just vals -> let num = head $ map (\val -> read val :: Int) (head vals) in
                                                                       putStrLn $ "Task 2: " ++ (show $ shortestDistDebug num child_map parent_map (head starts) (head ends))

handleFile :: String -> AP.ArgMap -> IO ()
handleFile input_filepath argMap = withFile input_filepath ReadMode (\handle -> fileHandler argMap handle)

handleArguments :: AP.ArgMap -> IO ()
handleArguments argMap = case Map.lookup "input_filepath" argMap of
                             Just x -> let input_filepath = (reverse x !! 0) !! 0 in
                                       handleFile input_filepath argMap
                             Nothing -> putStrLn "Couldn't get input_filepath"

argHelpHandler progArgs args = if AP.helpPresent progArgs args
                                   then AP.writeHelpText progArgs
                                   else let parse_result = AP.parseArguments progArgs args in
                                       case parse_result of
                                            Left msg -> putStrLn msg
                                            Right argMap -> handleArguments argMap

main = do
       args <- getArgs
       let progArgCont = AP.buildProgramArguments "Day template" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
