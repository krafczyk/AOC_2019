import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import qualified Library.Asm as Asm
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fileHandler handle = do
                     file_data <- hGetContents handle
                     let intcode_prog = map (\x -> read x :: Int) $ splitOn "," (lines file_data !! 0)
                     let task_1_prog = U.replaceNth 1 12 $ U.replaceNth 2 2 intcode_prog
                         final_state = (take 1 $ reverse $ U.takeWhileInclusive Asm.advanceCondition $ iterate (>>=Asm.advanceState) (Just (0, task_1_prog))) !! 0
                     case final_state of
                         Nothing -> putStrLn "Task 1 failed!"
                         Just (idx, state) -> putStrLn $ "Task 1: " ++ show (state !! 0)

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
       let progArgCont = AP.buildProgramArguments "day_2 solves Advent of Code 2019 day 2" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
