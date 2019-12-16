import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split
import Data.List
import Control.Monad.Except

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Point = Point { gX :: Int, gY :: Int } deriving (Eq, Ord)

instance Show Point where
    show a = "{"++(show $ gX a)++"," ++(show $ gY a)++"}"

buildPoint :: Int -> Int -> Point
buildPoint x y = Point { gX=x, gY=y }

add :: Point -> Point -> Point
add a b = Point { gX=(a_x+b_x), gY=(a_y+b_y) }
    where a_x = gX a
          b_x = gX b
          a_y = gY a
          b_y = gY b

dist :: Point -> Point -> Int
dist a b = dx+dy
    where dx = abs ((gX a)-(gX b))
          dy = abs ((gY a)-(gY b))

dirMap :: Map.Map Int Point
dirMap = Map.fromList [(1, buildPoint 0 1), -- North
                       (2, buildPoint 0 (-1)), -- South 
                       (3, buildPoint (-1) 0), -- West
                       (4, buildPoint 1 0)] -- East

mapDir :: Map.Map Point Int
mapDir = Map.fromList [(buildPoint 0 1, 1), -- North
                       (buildPoint 0 (-1), 2), -- South
                       (buildPoint (-1) 0, 3), -- West
                       (buildPoint 1 0, 4)] -- East

dirs = Map.keys mapDir

buildNeighbors :: Point -> Set.Set Point
buildNeighbors cur = foldr (\d acc -> Set.insert (add d cur) acc) Set.empty dirs

data ErrorType = NotInOpenArea
               | NonContiguousArea
               | DroneFailure String
               | SearchFailure String

instance Show ErrorType where
    show NotInOpenArea = "The drone is not currently in the known open area."
    show NonContiguousArea = "Non-contiguous area found"
    show (DroneFailure msg) = "There was a drone failure: " ++ msg
    show (SearchFailure msg) = "There was a search failure: " ++ msg

type ErrorMonad = Either ErrorType

findAdjacent :: Point -> Set.Set Point -> Set.Set Point
findAdjacent pos set = Set.intersection (buildNeighbors pos) set

-- Helper data type for building distance map of contiguous area
data BdMapState = BdMapState { bdvisited :: Set.Set Point,
                               open :: Set.Set Point,
                               distMap :: Map.Map Point Int,
                               bdqueue :: [Point] } deriving (Show)

-- Initialize the bdMapState
initBdMapState :: Point -> Set.Set Point -> BdMapState
initBdMapState orig open = BdMapState { bdvisited=Set.singleton orig,
                                        open=open,
                                        distMap=Map.singleton orig 0,
                                        bdqueue=Set.toList (findAdjacent orig open) }

-- Step in the mapping process
expandBdMapState :: BdMapState -> BdMapState
expandBdMapState init_state
    | null q = init_state
    | otherwise = init_state { bdqueue=newQ,
                               bdvisited=newVis,
                               distMap=newMap }
    where q = bdqueue init_state -- initial queue
          next = head $ q -- next point to examine from the queue
          vis = bdvisited init_state -- prev-visited spots
          dMap = distMap init_state -- prev-dist map
          oset = open init_state
          from = head $ sortBy (\x y -> compare (dMap Map.! x) (dMap Map.! y)) (Set.toList $ findAdjacent next vis)
          newMap = Map.insert next ((dMap Map.! from)+1) dMap
          newVis = Set.insert next vis
          q1 = drop 1 q
          add_to_queue = Set.toList $ Set.filter (\p -> Set.notMember p newVis) $ Set.filter (\p -> not $ p `elem` q1) $ findAdjacent next oset
          newQ = q1 ++ add_to_queue

-- Build the distance map from an origin.
buildDistMap :: Point -> Set.Set Point -> ErrorMonad (Map.Map Point Int)
buildDistMap orig open
    | Set.notMember orig open = throwError NotInOpenArea
    | ((Map.keys . distMap) finalState) /= (Set.toList open) = throwError NonContiguousArea
    | otherwise = Right (distMap finalState)
    where initState = initBdMapState orig open
          finalState = head $ dropWhile (\x -> not . null $ bdqueue x) $ iterate (expandBdMapState) initState

--We might need this function for performance reasons when working with a larger map
--expandDistMap

data Drone = Drone { brain :: Asm.IntcodeState } deriving (Show)

initDrone :: Asm.IntcodeProgram -> Drone
initDrone program = Drone { brain=Asm.initState program [] }

moveDrone :: (Int, Drone) -> Asm.ComputeMonad (Int, Drone)
moveDrone (dir,drone) =
    case cnext_state of
        Left msg -> Left msg
        Right state -> Right $ (out, drone { brain=state { Asm.getOutput=[] } } )
    where init_state = (brain drone) { Asm.getInput=[dir] }
          cnext_state = Asm.runProgramTillNeedInput init_state
          next_state = let (Right ns) = cnext_state in ns
          out = head $ Asm.getOutput next_state

data SearchState = SearchState { ssvisited :: Set.Set Point,
                                 sswalls :: Set.Set Point,
                                 to_explore :: Set.Set Point,
                                 curPos :: Point,
                                 drone :: Drone,
                                 oxy_pos :: Set.Set Point} deriving (Show)

initSearchState :: Asm.IntcodeProgram -> SearchState
initSearchState program = SearchState { ssvisited=Set.singleton origin,
                                sswalls=Set.empty,
                                to_explore=buildNeighbors origin,
                                curPos=origin,
                                drone=init_drone,
                                oxy_pos=Set.empty}
    where origin = buildPoint 0 0
          init_drone = initDrone program

advanceDrone :: (Int,Drone) -> ErrorMonad (Int,Drone)
advanceDrone (dir,drone) =
    case moveDrone (dir,drone) of
        Left err -> throwError (DroneFailure $ show err)
        Right (out,ndrone) -> return (out,ndrone)

isFailure :: ErrorMonad a -> Bool
isFailure input =
    case input of
        Left x -> True
        Right y -> False

getFailure :: ErrorMonad a -> String
getFailure input =
    case input of
        Left msg -> show msg
        Right y -> "Couldn't get message"

searchProgram :: SearchState -> ErrorMonad SearchState
--searchProgram :: SearchState -> IO ()
searchProgram init_state
    | isFailure mcdMap = throwError (SearchFailure $ getFailure mcdMap)
    | isFailure mdestMap = throwError (SearchFailure $ getFailure mdestMap)
    | isFailure mdrone = throwError (SearchFailure $ getFailure mdrone)
    | otherwise =
        case ostat of
            -- We hit a wall. Didn't move.
            0 -> return (init_state { sswalls=(Set.insert mloc walls),
                                      to_explore=(Set.delete mloc exp),
                                      drone=ndrone} )
            -- We successfully moved.
            1 -> return (init_state { ssvisited=(Set.insert mloc vis),
                                      to_explore=Set.union new_neighbors (Set.delete mloc exp),
                                      curPos=mloc,
                                      drone=ndrone } )
            -- We successfully moved and found the oxygen system!
            2 -> return (init_state { ssvisited=(Set.insert mloc vis),
                                      to_explore=Set.union new_neighbors (Set.delete mloc exp),
                                      curPos=mloc,
                                      drone=ndrone,
                                      oxy_pos=Set.singleton mloc } )
    where vis = ssvisited init_state
          walls = sswalls init_state
          exp = to_explore init_state
          cur = curPos init_state
          dro = drone init_state
          val_steps = Map.filter (\x -> Set.notMember x walls) $ Map.map (add cur) dirMap 
          -- Build distance map from current position.
          mcdMap = buildDistMap cur (Set.union vis exp)
          cdMap = let (Right x) = mcdMap in x
          -- Determine closest explorable point
          next_dest = head $ sortBy (\a b -> compare (cdMap Map.! a) (cdMap Map.! b)) $ Set.toList exp
          -- Build distance map from the selected point
          mdestMap = buildDistMap next_dest (Set.union vis exp)
          destMap = let (Right x) = mdestMap in x
          -- Determine next step as that closest to the destination
          (dir, mloc) = head $ sortBy (\(_,p1) (_,p2) -> compare (destMap Map.! p1) (destMap Map.! p2)) $ Map.toList val_steps
          new_neighbors = Set.filter (\p -> Set.notMember p (Set.union exp (Set.union walls vis))) $ buildNeighbors mloc
          mdrone = advanceDrone (dir,dro)
          (ostat,ndrone) = let (Right x) = mdrone in x

continueExploring :: ErrorMonad SearchState -> Bool
continueExploring ess =
    case ess of
        Left msg -> False
        Right ss -> if (null $ to_explore ss) then False else True

buildMap :: Asm.IntcodeProgram -> (Set.Set Point, Point)
buildMap program =
    case fss of
        Left msg -> (Set.empty, buildPoint 0 0)
        Right final_ss -> (ssvisited final_ss, head $ Set.toList $ oxy_pos final_ss)
    where init_ss = initSearchState program
          fss = head $ dropWhile (continueExploring) $ iterate (>>=searchProgram) $ Right init_ss

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
        (areaMap, opos) = buildMap program
        cdMap = buildDistMap (buildPoint 0 0) areaMap
    case cdMap of
        Left msg -> putStrLn $ "Couldn't build distmap" ++ (show msg)
        Right dMap -> putStrLn $ "Task 1: oxygen station is " ++ (show $ (dMap Map.! opos)) ++ " steps away."
    let mcoMap = buildDistMap opos areaMap
        (Right coMap) = mcoMap
        max_dist = Map.foldr (max) 0 coMap
    case mcoMap of
        Left msg -> putStrLn $ "Couldn't build distmap" ++ (show msg)
        Right coMap -> putStrLn $ "Task 2: oxygen will take " ++ (show max_dist) ++ " minutes to fill"

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
    let progArgCont = AP.buildProgramArguments "day_15 solves Advent of Code 2019 day 15" argDefinitions
    case progArgCont of
        Left msg -> putStrLn msg
        Right progArgs -> argHelpHandler progArgs args
