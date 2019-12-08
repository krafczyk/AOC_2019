import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.List.Split
import Data.Char

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

numElem :: (Eq a) => a -> [a] -> Int
numElem v list = foldl (\acc x -> if x == v then acc+1 else acc) 0 list

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let image_data_raw = head $ lines file_data
                                image_data = map (digitToInt) image_data_raw
                                image_width = 25
                                image_height = 6
                                layer_size = image_width*image_height
                                image_data_by_layer = chunksOf layer_size image_data
                                layer_num_zeros = map (numElem 0) image_data_by_layer
                                find_min_zero_layer = foldl (\(min_idx, min_val) (cur_idx, cur_val) -> if cur_val < min_val then (cur_idx, cur_val) else (min_idx, min_val)) (0,head $ layer_num_zeros) (zip [0..] layer_num_zeros)
                                min_zero_layer = image_data_by_layer !! (fst find_min_zero_layer)
                                task_1_answer = (numElem 1 min_zero_layer)*(numElem 2 min_zero_layer) 
                            putStrLn $ "Task 1: " ++ (show task_1_answer)

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
       let progArgCont = AP.buildProgramArguments "day_8 solves Advent of Code 2019 day 8" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
