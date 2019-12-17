import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split
import Data.Char

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

getDisplayChar :: [String] -> (Int, Int) -> Char
getDisplayChar display (xidx, yidx) = (display !! yidx) !! xidx

isIntersection :: [String] -> (Int,Int) -> Bool
isIntersection display (xidx, yidx) =
    foldr (&&) True $ map (\p -> (getDisplayChar display p) == '#') check_list
    where check_list = [(xidx,yidx),
                        (xidx+1,yidx),
                        (xidx-1,yidx),
                        (xidx,yidx+1),
                        (xidx,yidx-1)]

intAlignment :: [String] -> Int
intAlignment display =
    sum $ map (uncurry (*)) intersections
    where h = length display
          w = length $ head display
          indices = (\x y -> (x,y)) <$> [1..(w-2)] <*> [1..(h-2)]
          intersections = foldr (\pair acc -> if isIntersection display pair then pair:acc else acc) [] indices

task1 :: Asm.IntcodeState -> IO ()
task1 state =
    do
    let output = Asm.getOutput state
        display = filter (\l -> not $ null l) $ lines $ map (chr) output
    putStrLn $ "Task 1: " ++ (show $ intAlignment display)

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let program = map (\x -> read x :: Int) $ splitOn ","  $ head $ lines file_data
        result = Asm.runProgramTillNeedInput (Asm.initState program [])
    case result of
        Left msg -> putStrLn $ "Failed to run program: " ++ (show msg)
        Right res_state -> task1 res_state

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
    let progArgCont = AP.buildProgramArguments "day_17 solves Advent of Code 2019 day 17." argDefinitions
    case progArgCont of
        Left msg -> putStrLn msg
        Right progArgs -> argHelpHandler progArgs args
