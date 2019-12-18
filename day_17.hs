import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split
import Data.Char
import Data.List

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

task1 :: Asm.IntcodeState -> [String]
task1 state =
    filter (\l -> not $ null l) $ lines $ map (chr) output
    where output = Asm.getOutput state

data PathElement = TurnLeft | TurnRight | Forward Int deriving (Eq)

instance Show PathElement where
    show TurnLeft = "L"
    show TurnRight = "R"
    show (Forward n) = (show n)

data PathFindingState = PathFindingState { curDir :: Int,
                                           curPos :: (Int, Int),
                                           prevPath :: [PathElement],
                                           disp :: [String] } deriving (Eq)

dirMap :: Map.Map Int Point
dirMap = Map.fromList [ (0, (0,-1)), -- North
                        (1, (1,0)), -- East
                        (2, (0,1)), -- South
                        (3, (-1,0)) ] -- West

charDir :: Map.Map Char Int
charDir = Map.fromList [ ('^', 0), --North
                         ('>', 1), --East
                         ('v', 2), --South
                         ('<', 3) ] --West

turnRight :: Int -> Int
turnRight i = (i+1) `mod` 4

turnLeft :: Int -> Int
turnLeft i = (i-1) `mod` 4

initPathFindingState :: [String] -> Point -> PathFindingState
initPathFindingState display orig =
    PathFindingState { curDir=charDir Map.! (getDisplayChar display orig),
                       curPos=orig,
                       prevPath=[],
                       disp=display }

type Point = (Int, Int)

add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

mult :: Point -> Int -> Point
mult (x,y) m = (m*x,m*y)

isInboundsRoad :: [String] -> Point -> Int -> Bool
isInboundsRoad display loc dir
    | cx < 0 || cx >= w = False
    | cy < 0 || cy >= h = False
    | getDisplayChar display (cx,cy) == '#' = True
    | otherwise = False
    where h = length display
          w = length $ head display
          (cx,cy) = (add loc (dirMap Map.! dir))

pathFindingImp :: PathFindingState -> PathFindingState
pathFindingImp in_ps
    | isInboundsRoad display curpos curdir = in_ps { curPos=newpos, prevPath=(Forward num_advance:prev_path) }
    | isInboundsRoad display curpos leftdir = in_ps { curDir=leftdir, prevPath=(TurnLeft:prev_path) }
    | isInboundsRoad display curpos rightdir = in_ps { curDir=rightdir, prevPath=(TurnRight:prev_path) }
    | otherwise = in_ps
    where display = disp in_ps
          curpos = curPos in_ps
          curdir = curDir in_ps
          leftdir = turnLeft curdir
          rightdir = turnRight curdir
          prev_path = prevPath in_ps
          num_advance = head $ dropWhile (\i -> isInboundsRoad display (add curpos (mult (dirMap Map.! curdir) i)) curdir) [1..]
          newpos = add curpos (mult (dirMap Map.! curdir) num_advance)

pathFinding :: [String] -> [PathElement]
pathFinding display =
    prevPath final_ps
    where h = length display
          w = length $ head display
          indices = (\x y -> (x,y)) <$> [0..(w-1)] <*> [0..(h-1)]
          orig = head $ filter (\p -> (getDisplayChar display p) `elem` ['^','<','>','v']) indices
          init_ps = initPathFindingState display orig
          final_ps = head $ dropWhile (\ps -> ps /= (pathFindingImp ps)) $ iterate (pathFindingImp) init_ps

furtherProcessing :: Asm.IntcodeState -> Asm.IntcodeProgram -> IO ()
furtherProcessing state program =
    do
    let display = task1 state
    putStrLn $ "Task 1: " ++ (show $ intAlignment display)
    print $ pathFinding display

buildSubsetsSingle :: (Eq a) => [a] -> Int -> [[a]]
buildSubsetsSingle list len
    | len > length list = []
    | otherwise = nub subsets -- Return only unique subsets.
    where num = (length list) - len + 1
          idxs = take num [0..]
          subsets = foldl (\acc idx -> (take len $ drop idx list):acc) [] idxs

buildSubsets :: (Eq a) => [[a]] -> Int -> [[a]]
buildSubsets lists len =
    foldr (\l acc -> (buildSubsetsSingle l len) ++ acc) [] lists

numOccurancesSingle :: (Eq a) => [a] -> [a] -> Int
numOccurancesSingle list sublist =
    foldl (\acc i -> if (take len2 $ drop i list) == sublist then acc+1 else acc) 0 (take num_pos [0..])
    where len1 = length list
          len2 = length sublist
          num_pos = len1-len2+1 

numOccurances :: (Eq a) => [[a]] -> [a] -> Int
numOccurances lists sublist =
    sum $ map (\l -> numOccurancesSingle l sublist) lists

getBestSubseq :: (Eq a) => [a] -> [a]
getBestSubseq to_digest =
    fst result
    where init_list = [to_digest]
          best_len = fst $ foldr1 (\(l,s) (ml,ms) -> if s > ms then (l,s) else (ml,ms)) $ foldr (\l1 acc -> (l1, l1*(maximum $ foldr (\subl acc1 -> (numOccurances init_list subl):acc1) [] (buildSubsets init_list l1))):acc) [] [2,4..20]
          result = foldr (\(l,n) (ml,mn) -> if n > mn then (l,n) else (ml,mn)) ([],0) $ map (\sl -> (sl, numOccurances init_list sl)) (buildSubsets init_list best_len)

subseqLocations :: (Eq a) => [a] -> [a] -> [Int]
subseqLocations list sublist =
    foldl (\acc i -> if (take l2 $ drop i list) == sublist then acc++[i] else acc) [] (take num [0..])
    where l1 = length list
          l2 = length sublist
          num = l1-l2+1

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    --let program = map (\x -> read x :: Int) $ splitOn ","  $ head $ lines file_data
    --    result = Asm.runProgramTillNeedInput (Asm.initState program [])
    --case result of
    --    Left msg -> putStrLn $ "Failed to run program: " ++ (show msg)
    --    Right res_state -> furtherProcessing res_state
    -- pathFinding Testing
    let display = lines file_data
        found_path = reverse $ pathFinding display
    mapM_ (putStrLn) display
    print $ found_path
    print $ length found_path
    --let num = 18
    --print $ buildSubsets found_path num
    --print $ foldr (\subl acc -> ((numOccurances found_path subl):acc)) [] (buildSubsets found_path num)
    --print $ length $ buildSubsets found_path num
    --print $ foldr1 (\(l,s) (ml,ms) -> if s > ms then (l,s) else (ml,ms)) $ foldr (\l1 acc -> (l1, l1*(maximum $ foldr (\subl acc1 -> (numOccurances found_path subl):acc1) [] (buildSubsets found_path l1))):acc) [] [2,4..20]
    print $ getBestSubseq found_path
    print $ subseqLocations found_path $ getBestSubseq found_path

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
