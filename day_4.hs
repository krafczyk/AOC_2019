import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.List.Split
import Data.Char

argDefinitions = [ ("range", ["-r", "--range"], "Number range for password generation", 1) ]

asString :: [Int] -> [String]
asString input = map (show) input

asIntList :: [String] -> [Int]
asIntList input = map (\x -> read x :: Int) input

adjacency :: String -> Bool
adjacency str = foldr (||) False $ foldr (\(x,y) acc -> if x==y then (True:acc) else (False:acc)) [] (zip str (drop 1 str))

monotonic :: String -> Bool
monotonic str = foldr (&&) True $ foldr (\(x,y) acc -> if ord x <= ord y then (True:acc) else (False:acc)) [] (zip str (drop 1 str))

--task2 :: String -> Bool
task2 str = foldl (\acc n -> if n == 2 then True||acc else False||acc) False $ foldl (\acc c -> Map.unionWith (+) acc (Map.singleton c 1) ) Map.empty str

handleRange :: String -> IO ()
handleRange "" = do
                 putStrLn "No range to solve problem"
handleRange range = do
                    let [low,high] = asIntList $ splitOn "-" range
                        all_possibilities = take (high-low+1) [low..]
                        first_cut_poss = filter monotonic $ filter adjacency $ asString all_possibilities
                    putStrLn $ "Task 1: " ++ (show $ length $ first_cut_poss)
                    putStrLn $ "Task 2: " ++ (show $ length $ filter task2 first_cut_poss)

argHelpHandler progArgs args = if AP.helpPresent progArgs args
                                   then AP.writeHelpText progArgs
                                   else let parse_result = AP.parseArguments progArgs args in
                                       case parse_result of
                                            Left msg -> putStrLn msg
                                            Right argMap -> case Map.lookup "range" argMap of
                                                                Just x -> let range = (reverse x !! 0) !! 0 in
                                                                          handleRange range
                                                                Nothing -> handleRange ""

main = do
       args <- getArgs
       let progArgCont = AP.buildProgramArguments "Day 4 for Advent of Code 2019" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
