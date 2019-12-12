import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.List
import Data.List.Split
import Data.Char

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Vector = Vector { gX :: Int, gY :: Int, gZ :: Int } deriving (Eq)

instance Show Vector where
    show p = (show $ gX p) ++ "," ++ (show $ gY p) ++ "," ++ (show $ gZ p)

-- <x=-1, y=0, z=2>
-- <x=2, y=-10, z=-7>
-- <x=4, y=-8, z=8>
-- <x=3, y=5, z=-1>

instance Read Vector where
    readsPrec _ value
        | mbegin_idx == Nothing = []
        | mend_idx == Nothing = []
        | length split_1 /= 3 = []
        | map (length) split_2 /= [2,2,2] = []
        | map (head) split_2 /= ["x", "y", "z"] = []
        | (length $ filter (\l -> not $ null l) v_parses) /= 3 = []
        | otherwise = [ (Vector { gX=x, gY=y, gZ=z }, rest) ]
        -- We attempt to progressively read the input value.
        where mbegin_idx = findIndex (=='<') value
              mend_idx = findIndex (=='>') value
              begin_idx = let (Just bidx) = mbegin_idx in bidx
              end_idx = let (Just eidx) = mend_idx in eidx
              selected = filter (not . isSpace) $ take (end_idx-(begin_idx+1)) $ drop (begin_idx+1) value
              rest = drop (end_idx+1) value
              split_1 = splitOn "," selected
              split_2 = map (splitOn "=") split_1
              nums = map (head . (drop 1)) split_2
              v_parses = map (\x -> reads x :: [(Int,String)]) nums
              [x,y,z] = map (\v -> read v :: Int) nums

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let planet_lines = lines file_data
                                planets = map (\l -> read l :: Vector) planet_lines
                            mapM_ (putStrLn) planet_lines -- Read problem data here
                            mapM_ (print) planets

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
       let progArgCont = AP.buildProgramArguments "day_12 Solves Advent of Code 2019 day 12" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
