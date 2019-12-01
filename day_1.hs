import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fuelConvert :: Int -> Int
fuelConvert mass = (mass `div` 3) - 2

handleFile :: String -> IO ()
handleFile input_filepath = withFile input_filepath ReadMode (\handle -> do
                                                                         file_data <- hGetContents handle
                                                                         let mass_data = map (\x -> read x :: Int) $ lines file_data
                                                                             fuel_data = map fuelConvert mass_data
                                                                         putStrLn $ "Task 1: " ++ (show $ sum fuel_data))

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
       let progArgCont = AP.buildProgramArguments "Day template" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
