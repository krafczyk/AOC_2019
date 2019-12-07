import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split
import Data.List

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

runAmplifier :: Asm.IntcodeProgram -> Int -> Int -> Asm.ComputeMonad Int
runAmplifier program phase input = case Asm.runProgram [phase, input] program of
                                       Left x -> Left x
                                       Right (_, output, _, _) -> Right (head output)

runAmplifiers :: Asm.IntcodeProgram -> [Int] -> Int -> Asm.ComputeMonad Int
runAmplifiers program sequence input = foldl (\acc phase -> acc >>= (runAmplifier program phase)) (Right input) sequence

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
                                sequences = permutations [0,1,2,3,4]
                                task_1_results = sequence $ map (\sequ -> runAmplifiers program sequ 0) sequences
                            case task_1_results of
                                Left msg -> putStrLn $ "Task 1 failed with error " ++ (show msg)
                                Right vals -> putStrLn $ "Task 1: " ++ (show $ maximum vals)

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
