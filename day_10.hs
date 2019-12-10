import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP

data Point = Point { getX :: Int, getY :: Int } deriving (Show, Eq)

buildAsteroidList :: [String] -> [Point]
buildAsteroidList asteroid_map =
    foldl (\acc (line, y_idx) ->
        let line_asteroids = foldl (\acc (c, x_idx) -> if c == '#' then acc ++ [Point { getX=x_idx, getY=y_idx}] else acc) [] (zip line [0..]) in
        if line_asteroids == []
            then acc
            else acc ++ line_asteroids) [] line_enum
    where line_enum = zip asteroid_map [0..]

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let asteroid_map = lines file_data
                            mapM_ putStrLn asteroid_map -- Read problem data here
                            let asteroid_list = buildAsteroidList asteroid_map
                            print $ asteroid_list
                            print $ length asteroid_list

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
       let progArgCont = AP.buildProgramArguments "day_10 solves Advent of Code 2019 Day 10" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
