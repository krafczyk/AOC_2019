import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

digestOutput :: [Int] -> [(Int, Int, Int)]
digestOutput [x,y,t] = [(x,y,t)]
digestOutput (x:y:t:ps) = (x,y,t):digestOutput ps

task1Stuff :: Asm.IntcodeState -> IO ()
task1Stuff final_state =
    do
    let outputs = Asm.getOutput final_state
        screen_data = digestOutput outputs
        screen_map :: Map.Map (Int,Int) Int
        screen_map = foldl (\acc (x,y,t)-> Map.insert (x,y) t acc) Map.empty screen_data
        num_block_tiles = length $ Map.keys $ Map.filter (\t -> t == 2) screen_map
    putStrLn $ "Task 1: " ++ (show num_block_tiles)

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
                                result = Asm.runProgram [] program
                            case result of
                                Left msg -> putStrLn $ "Program Failed! " ++ (show msg)
                                Right final_state -> task1Stuff final_state

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
       let progArgCont = AP.buildProgramArguments "day_13 solves Advent of Code 2019 day 13" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
