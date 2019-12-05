import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fileHandler handle = do
                     file_data <- hGetContents handle
                     let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
                     case Asm.runProgram [1] program of
                         Left msg -> putStrLn $ "Running program for task 1 failed. (" ++ (show msg) ++ ")"
                         Right (_, outputs, _, _) -> let rev_out = reverse outputs
                                                         diagnostic_code = head rev_out
                                                         test_codes = drop 1 rev_out in
                                                     if foldl1 (&&) $ map (==0) test_codes
                                                         then do
                                                              putStrLn "Tests passed!"
                                                              putStrLn $ "Task 1: " ++ (show diagnostic_code)
                                                         else do 
                                                              putStrLn "Tests Failed!"
                                                              putStrLn $ show rev_out
                                                              putStrLn $ show test_codes
                     case Asm.runProgram [5] program of
                         Left msg -> putStrLn $ "Running program for task 2 failed. (" ++ (show msg) ++ ")"
                         Right (_, outputs, _, _) -> putStrLn $ "Task 2: " ++ (show $ head outputs)
                     --putStrLn $ show $ Asm.runProgram [-20] program

handleFile :: String -> IO ()
handleFile input_filepath = withFile input_filepath ReadMode fileHandler

argHelpHandler progArgs args = if AP.helpPresent progArgs args
                                   then AP.writeHelpText progArgs
                                   else let parse_result = AP.parseArguments progArgs args in
                                       case parse_result of
                                            Left msg -> putStrLn msg
                                            Right argMap -> case Map.lookup "input_filepath" argMap of
                                                                Just x -> let input_filepath = (reverse x !! 0) !! 0 in
                                                                          handleFile input_filepath
                                                                Nothing -> putStrLn "Couldn't get input_filepath"

main = do
       args <- getArgs
       let progArgCont = AP.buildProgramArguments "Solves Advent of code 2019 Day 5" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
