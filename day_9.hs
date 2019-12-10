import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
                                result = Asm.runProgram [1] program -- Read problem data here
                            case result of
                                Left msg -> putStrLn $ show $ "Error: " ++ (show msg)
                                Right res ->
                                    let output = Asm.getOutput res in
                                    if length output == 1
                                        then putStrLn $ "Task 1: " ++ (show $ head $ output)
                                        else putStrLn $ "Task 1 failed."
                            let result2 = Asm.runProgram [2] program
                            case result2 of
                                Left msg -> putStrLn $ show $ "Error: " ++ (show msg)
                                Right res ->
                                    let output = Asm.getOutput res in
                                    putStrLn $ "Task 2: " ++ (show $ head output)

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
       let progArgCont = AP.buildProgramArguments "day_9 solves Advent of code 2019 day 8" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
