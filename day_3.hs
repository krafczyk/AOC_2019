import System.Environment
import System.IO
import qualified Data.Map as Map
import Data.List.Split
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import qualified Data.Set as S

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Move = MoveRight Int | MoveLeft Int | MoveUp Int | MoveDown Int deriving (Show)

buildMoveSub :: (Int -> Move) -> String -> Maybe Move
buildMoveSub moveConst input = case (U.readMaybe input :: Maybe Int) of
                                   Nothing -> Nothing
                                   Just x -> Just (moveConst x)

buildMove :: String -> Maybe Move
buildMove ('R':xs) = buildMoveSub (MoveRight) xs
buildMove ('L':xs) = buildMoveSub (MoveLeft) xs
buildMove ('U':xs) = buildMoveSub (MoveUp) xs
buildMove ('D':xs) = buildMoveSub (MoveDown) xs
buildMove _ = Nothing

type Point = (Int, Int)

applyMove :: Move -> [Point] -> [Point]
applyMove (MoveRight 1) ((x,y):ps) = (x+1,y):(x,y):ps
applyMove (MoveRight v) ((x,y):ps) = (x+v,y):(applyMove (MoveRight (v-1)) ((x,y):ps))
applyMove (MoveLeft 1) ((x,y):ps) = (x-1,y):(x,y):ps
applyMove (MoveLeft v) ((x,y):ps) = (x-v,y):(applyMove (MoveLeft (v-1)) ((x,y):ps))
applyMove (MoveUp 1) ((x,y):ps) = (x,y+1):(x,y):ps
applyMove (MoveUp v) ((x,y):ps) = (x,y+v):(applyMove (MoveUp (v-1)) ((x,y):ps))
applyMove (MoveDown 1) ((x,y):ps) = (x,y-1):(x,y):ps
applyMove (MoveDown v) ((x,y):ps) = (x,y-v):(applyMove (MoveDown (v-1)) ((x,y):ps))

distManhattan :: Point -> Int
distManhattan (x,y) = (abs x)+(abs y)

distToInt :: Point -> [Point] -> Int
distToInt p points = length $ takeWhile (p/=) points

solveProblem :: [Move] -> [Move] -> IO ()
solveProblem a b = do
                   let a_points = foldl (\acc x -> applyMove x acc) [(0,0)] a
                       b_points = foldl (\acc x -> applyMove x acc) [(0,0)] b
                       a_list = reverse a_points
                       b_list = reverse b_points
                       a_set = S.fromList a_points
                       b_set = S.fromList b_points
                       int_list = S.toList $ S.delete (0,0) $ S.intersection a_set b_set
                       closest_int_dist = minimum $ map distManhattan int_list
                   putStrLn $ "Task 1: " ++ show closest_int_dist
                   let dists = map (\int -> (distToInt int a_list,distToInt int b_list)) $ int_list
                       closest_walk_dist = minimum $ map (uncurry (+)) dists
                   putStrLn $ "Task 2: " ++ show closest_walk_dist

fileHandler handle = do
                     file_data <- hGetContents handle
                     let split_lines = map (splitOn ",") $ lines file_data
                         build_moves = map (map buildMove) split_lines
                         jmove_sequences = sequence $ map sequence build_moves
                     case jmove_sequences of
                        Nothing -> putStrLn "Couldn't build move sequence!"
                        Just [a,b] -> solveProblem a b
                        Just _ -> putStrLn "More sequences than supported!"

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
       let progArgCont = AP.buildProgramArguments "day_3 solves Advent of Code 2019 day 3" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
