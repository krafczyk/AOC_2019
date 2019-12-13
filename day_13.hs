import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import qualified Library.Utility as U
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

digestOutput :: [Int] -> [(Int, Int, Int)]
digestOutput [x,y,t] = [(x,y,t)]
digestOutput (x:y:t:ps) = (x,y,t):digestOutput ps

buildScreenMap :: [(Int, Int, Int)] -> Map.Map (Int, Int) Int
buildScreenMap screen_data = foldl (\acc (x,y,t)-> Map.insert (x,y) t acc) Map.empty screen_data

charMap :: Map.Map Int Char
charMap = Map.fromList [(0, ' '), -- empty tile
                        (1, '#'), -- wall tile
                        (2, '~'), -- block tile
                        (3, '_'), -- paddle tile
                        (4, 'o') ] -- ball tile

isScore :: (Int, Int) -> Int -> Bool
isScore (x, y) _ = (x == -1) && (y == 0)

-- x = 0-39
-- y = 0-20
drawMainScreen :: Map.Map (Int, Int) Int -> [String]
drawMainScreen screen_data =
    foldr (\yidx acc -> (foldr (\xidx acc2 -> (charMap Map.! (screen_data Map.! (xidx,yidx))):acc2) [] xidxs):acc) [] yidxs
    where yidxs = [0..20]
          xidxs = [0..39]

drawScreen :: Map.Map (Int, Int) Int -> [String]
drawScreen screen_data
    | length score_data == 0 = "Score: 0":drawMainScreen screen_data
    | otherwise = ("Score: " ++ (show $ snd $ head score_data)):drawMainScreen screen_data
    where score_data = Map.toList $ Map.filterWithKey (\(x,y) t -> isScore (x,y) t) screen_data

task1Stuff :: Asm.IntcodeState -> IO ()
task1Stuff final_state =
    do
    let outputs = Asm.getOutput final_state
        screen_data = digestOutput outputs
        screen_map = buildScreenMap screen_data
        num_block_tiles = length $ Map.keys $ Map.filter (\t -> t == 2) screen_map
    putStrLn $ "Task 1: " ++ (show num_block_tiles)
    mapM_ (putStrLn) $ drawScreen screen_map

ballPosition :: Map.Map (Int, Int) Int -> (Int, Int)
ballPosition screen_map = Map.foldrWithKey (\(x,y) t acc -> if t == 4 then (x,y) else acc) (-1,-1) screen_map

paddlePosition :: Map.Map (Int, Int) Int -> (Int, Int)
paddlePosition screen_map = Map.foldrWithKey (\(x,y) t acc -> if t == 3 then (x,y) else acc) (-1,-1) screen_map

getScore :: Map.Map (Int, Int) Int -> Int
getScore screen_map = Map.foldrWithKey (\(x,y) t acc -> if (isScore (x,y) t) then t else acc) 0 screen_map

continuePlaying :: Asm.ComputeMonad Asm.IntcodeState -> Bool
continuePlaying gstate =
    case gstate of
        Left msg -> False
        Right state -> if opmode == 99 then False else True
    where game_state = let (Right state) = gstate in state
          idx = Asm.getIdx game_state
          program = Asm.getProgram game_state
          opmode = program !! idx

playGame :: Asm.IntcodeState -> Asm.ComputeMonad Asm.IntcodeState
playGame cur_state =
    case packed_state of
        Left msg -> Left msg
        Right state ->
            if ball_x > paddle_x
                then Right $ next_state { Asm.getInput=[1] }
                else if ball_x < paddle_x
                        then Right $ next_state { Asm.getInput=[-1] }
                        else Right $ next_state { Asm.getInput=[0] }
    where packed_state = Asm.runProgramTillNeedInput cur_state
          next_state = let (Right state) = packed_state in state
          outputs = Asm.getOutput next_state
          screen_map = buildScreenMap $ digestOutput outputs
          paddle_x = fst $ paddlePosition screen_map
          ball_x = fst $ ballPosition screen_map

task2Stuff :: Asm.IntcodeProgram -> IO ()
task2Stuff program =
    do 
    let init_state = Asm.initState program []    
        fstate = head $ dropWhile (continuePlaying) $ iterate (>>=playGame) (Right init_state)
    case fstate of
        Left msg -> putStrLn $ "Task 2 failed!: " ++ (show msg)
        Right final_state ->
            do
            let final_outputs = Asm.getOutput final_state
                final_map = buildScreenMap $ digestOutput final_outputs
                final_score = getScore final_map
            putStrLn $ "Task 2: " ++ (show final_score)
            mapM_ (putStrLn) $ drawScreen final_map

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
        result = Asm.runProgram [] program
    case result of
         Left msg -> putStrLn $ "Program Failed! " ++ (show msg)
         Right final_state -> task1Stuff final_state
    -- Set memory at location 0 to 2 to 'play' the game.
    let program2 = U.replaceNth 0 2 program
    task2Stuff program2
                                

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
       let progArgCont = AP.buildProgramArguments "day_13 solves Advent of Code 2019 day 13" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
