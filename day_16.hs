import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.Char

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

base_pattern :: [Int]
base_pattern = [0, 1, 0, -1]

apply_pattern :: Int -> [Int] -> [Int]
apply_pattern len input = foldr (\i acc -> ((abs $ foldr (\(j,v) acc1-> acc1+((base_pattern !! (((j+1) `div` i) `mod` 4))*v) ) 0 (zip [0..] input)) `mod` 10):acc) [] (take len [1..])

run_patterns :: [Int] -> Int -> [Int]
run_patterns message_data num = head $ drop num $ iterate (\x -> apply_pattern (length message_data) x) message_data
    --where masks = build_patterns (length message_data)

get8Digits :: Int -> [Int] -> String
get8Digits offset message_data =
    map (\c -> chr ((ord '0')+c)) $ take 8 $ drop offset message_data

task1 :: [Int] -> Int -> String
task1 message_data num =
    get8Digits 0 $ run_patterns message_data num

task2 :: [Int] -> Int -> String
task2 message_data num = get8Digits message_offset $ run_patterns (take (10000*(length message_data)) $ cycle message_data) num
    where message_offset = sum $ map (\(v,e) -> v*(10^e)) $ zip (reverse $ take 7 message_data) [0..]

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let message_data = map (\x -> (ord x)-(ord '0')) $ head $ lines file_data
    putStrLn $ "Task 1: " ++ (task1 message_data 100)
    --putStrLn $ "Task 2: " ++ (task2 message_data 2)

handleFile :: String -> AP.ArgMap -> IO ()
handleFile input_filepath argMap = withFile input_filepath ReadMode (fileHandler argMap)

handleArguments :: AP.ArgMap -> IO ()
handleArguments argMap =
    case Map.lookup "input_filepath" argMap of
        Just x -> let input_filepath = (reverse x !! 0) !! 0 in
                  handleFile input_filepath argMap
        Nothing -> putStrLn "Couldn't get input_filepath"

argHelpHandler progArgs args =
    if AP.helpPresent progArgs args
        then AP.writeHelpText progArgs
        else let parse_result = AP.parseArguments progArgs args in
             case parse_result of
                 Left msg -> putStrLn msg
                 Right argMap -> handleArguments argMap

main =
    do
    args <- getArgs
    let progArgCont = AP.buildProgramArguments "day_16 solves Advent of Code 2019 day 16" argDefinitions
    case progArgCont of
        Left msg -> putStrLn msg
        Right progArgs -> argHelpHandler progArgs args
