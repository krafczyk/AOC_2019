import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import qualified Library.Utility as U
import Data.List.Split
import Data.List

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Point = Point { getX :: Int, getY :: Int } deriving (Eq)

showNice :: Point -> String
showNice a = (show x) ++ "," ++ (show y)
    where x = getX a
          y = getY a

instance Show Point where
    show a = "p=("++(show x) ++ "," ++ (show y) ++ ")"
        where x = getX a
              y = getY a

instance Ord Point where
    compare a b
        | a_y > b_y = GT
        | a_y < b_y = LT
        | a_x > b_x = GT
        | a_x < b_x = LT
        | otherwise = EQ
        where a_x = getX a
              a_y = getY a
              b_x = getX b
              b_y = getY b

buildPoint :: Int -> Int -> Point
buildPoint x y = Point { getX=x, getY=y }

add :: Point -> Point -> Point
add a b = Point { getX=(getX a+getX b), getY=(getY a+getY b) }

dirMap :: Map.Map Int Point
dirMap = Map.fromList [ (0, (buildPoint 0 1)),
                        (1, buildPoint 1 0),
                        (2, buildPoint 0 (-1)),
                        (3, buildPoint (-1) 0) ]

turnRight :: Int -> Int
turnRight dir = (dir+1) `mod` 4

turnLeft :: Int -> Int
turnLeft dir = (dir-1) `mod` 4

data RobotState = RobotState { history :: [(Point,Int)], location :: Point, direction :: Int, brain :: Asm.ComputeMonad Asm.IntcodeState } deriving (Show)

initRoboState :: Asm.IntcodeProgram -> RobotState
initRoboState program = RobotState { history=[],
                                    location=(buildPoint 0 0),
                                    direction=0,
                                    brain = Right (Asm.initState program []) }

advanceProgramSupplyInput :: Asm.IntcodeInputs -> Asm.IntcodeState -> Asm.ComputeMonad Asm.IntcodeState
advanceProgramSupplyInput input init_state = last $ U.takeWhileInclusive Asm.advanceConditionWaitInput $ iterate (>>=Asm.advanceState) (Right start_state)
    where start_state = init_state { Asm.getInput=input }

continueRunning robo_state =
    case cbrain of
        Left x -> False
        Right state -> if (opmode == 99) then False else True
            where idx = Asm.getIdx state
                  program = Asm.getProgram state
                  opmode = program !! idx
    where cbrain = brain robo_state

advanceRobot robo_state =
    case new_brain of
        Left x -> robo_state { brain=new_brain }
        Right new_state ->
            robo_state { history=(cur_loc,new_color):hist,
                         direction=new_dir,
                         location=add cur_loc (dirMap Map.! new_dir),
                         brain=Right (new_state { Asm.getOutput=[] }) }
            where outputs = Asm.getOutput new_state
                  new_color = head outputs
                  turn_inst = head $ drop 1 outputs
                  new_dir = if turn_inst == 0 then turnLeft dir else turnRight dir
    where cur_loc = location robo_state
          hist = history robo_state
          paint_hist = foldr (\(p, val) acc -> if p == cur_loc then val:acc else acc) [] hist
          cur_color = if null paint_hist then 0 else head paint_hist
          init_brain = brain robo_state
          dir = direction robo_state
          new_brain = init_brain >>= (advanceProgramSupplyInput [cur_color])

extentFold :: Point -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
extentFold p (min_x, max_x, min_y, max_y) =
    (min_x', max_x', min_y', max_y')
    where x = getX p
          y = getY p
          min_x' = min x min_x
          max_x' = max x max_x
          min_y' = min y min_y
          max_y' = max y max_y

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
                                robo = initRoboState program
                            let hist =  U.takeWhileInclusive (continueRunning) $ iterate (advanceRobot) robo
                                final_state = last hist
                                final_history = history final_state
                                positions_visited = map (fst) $ final_history
                                unique_positions_visited = nub positions_visited
                            putStrLn $ "Task 1: " ++ (show $ length unique_positions_visited)
                            let roboi = initRoboState program
                                robo2 = roboi { history=[(buildPoint 0 0, 1)] }
                                hist = U.takeWhileInclusive (continueRunning) $ iterate (advanceRobot) robo2
                                final_state = last hist
                                final_history = history final_state
                                history_map :: Map.Map Point [Int]
                                history_map =
                                    foldr (\(p,c) acc -> Map.unionWith (++) (Map.singleton p [c]) acc) Map.empty final_history
                                paint_map :: Map.Map Point Int
                                paint_map =
                                    foldr (\(p,c) acc -> Map.insert p c acc) Map.empty final_history
                                unique_locations = Map.keys paint_map
                                (min_x, max_x, min_y, max_y) =
                                    foldr (extentFold) (0,0,0,0) unique_locations
                                width = max_x-min_x+1
                                height = max_y-min_y+1
                                y_idxs = [max_y,(max_y-1)..min_y]
                                x_idxs = [min_x .. max_x]
                                decal :: [String]
                                decal =
                                    foldr (\y_idx acc ->
                                        (foldr (\x_idx acc2 ->
                                            case Map.lookup (buildPoint x_idx y_idx) paint_map of
                                                Nothing -> ' ':acc2
                                                Just i -> if i == 0 then ' ':acc2 else '#':acc2) "" x_idxs):acc
                                    ) [] y_idxs
                            putStrLn $ "Task 2:"
                            mapM_ (putStrLn) decal


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
       let progArgCont = AP.buildProgramArguments "day_11 solves Advent of Code 2019 day 11" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
