import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import qualified Library.Asm as Asm
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

setNounVerb :: Int -> Int -> [Int] -> [Int]
setNounVerb n v state = U.replaceNth 1 n $ U.replaceNth 2 v $ state

runProgramWNV :: (Int,Int) -> [Int] -> Asm.ComputeMonad Int
runProgramWNV (n,v) state = case Asm.runProgram [] $ setNounVerb n v state of
                                Left x -> Left x
                                Right (_, _, _, state) -> Right (state !! 0)

reportResult :: ((Int, Int), Asm.ComputeMonad Int) -> String
reportResult (_,Left x) = "Task 2 Failed. msg: " ++ (show x)
reportResult ((n,v), k) = "Task 2: " ++ show (100*n+v)

fileHandler handle = do
                     file_data <- hGetContents handle
                     let intcode_prog = map (\x -> read x :: Int) $ splitOn "," (lines file_data !! 0)
                     let task_1_prog = setNounVerb 12 2 intcode_prog
                     case Asm.runProgram [] task_1_prog of
                         Left msg -> putStrLn $ "Task 1 failed! msg: " ++ (show msg)
                         Right (_, _, _, state) -> putStrLn $ "Task 1: " ++ (show $ head state)
                     let noun_verb_pairs = [(n,v) | n <- take 100 [0..], v <- take 100 [0..]]
                         results = U.takeWhileInclusive (\(_,v) -> v /= (Right 19690720)) $ map (\x -> (x, runProgramWNV x intcode_prog)) noun_verb_pairs
                         target_result = (take 1 $ reverse $ results) !! 0
                     putStrLn $ reportResult target_result

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
