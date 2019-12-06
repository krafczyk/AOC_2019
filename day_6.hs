import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.List.Split
import Data.Typeable

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

type OrbitMap = Map.Map String [String]

countOrbits1 :: OrbitMap -> Int
countOrbits1 orbitMap = sum $ map (numDescendents orbitMap) orbiters
    where orbiters = Map.keys orbitMap

numDescendents :: OrbitMap -> String -> Int
numDescendents orbitMap node
    | children == [] = 0 -- No children
    | otherwise = length children + (sum vals)
    where children = case Map.lookup node orbitMap of
                          Nothing -> []
                          Just x -> x
          vals = map (numDescendents orbitMap) children

shortestDist :: OrbitMap -> String -> String -> Int
shortestDist orbitMap start end = 0

fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let orbit_map = foldl1 (Map.unionWith (++)) $ map (\[x,y] -> Map.singleton y [x]) $ map (splitOn ")") $ lines file_data
                            putStrLn $ "Task 1: " ++ (show $ countOrbits1 orbit_map)
                            let init = sequence [Map.lookup "YOU" orbit_map, Map.lookup "SAN" orbit_map]
                            case init of
                                Nothing -> putStrLn "Can't solve part 2! Can't find either start or end points!"
                                Just [starts, ends] -> putStrLn $ "Task 2: " ++ (show $ shortestDist orbit_map (head starts) (head ends))


handleFile :: String -> AP.ArgMap -> IO ()
handleFile input_filepath argMap = withFile input_filepath ReadMode (fileHandler argMap)

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
