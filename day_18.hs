import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Library.ArgParser as AP
import Library.Vec
import Library.Maze
import Data.Char
import Data.List
import Data.Maybe

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Point = Point { gX :: Int, gY :: Int } deriving (Eq, Ord)

point :: Int -> Int -> Point
point x y = Point { gX=x, gY=y }

instance Show Point where
    show a = "{" ++ (show a_x) ++ "," ++ (show a_y) ++ "}"
        where a_x = gX a
              a_y = gY a

instance VecI Point where
    dist a b = abs(a_x-b_x)+abs(a_y-b_y)
        where a_x = gX a
              b_x = gX b
              a_y = gY a
              b_y = gY b
    dot a b = (a_x*b_x)+(a_y*b_y)
        where a_x = gX a
              b_x = gX b
              a_y = gY a
              b_y = gY b
    mag a = dist a a

instance MappableI Point where
    neighbors a = [ point a_x (a_y+1),
                    point (a_x+1) a_y,
                    point a_x (a_y-1),
                    point (a_x-1) a_y ]
        where a_x = gX a
              a_y = gY a

data CaveState = CaveState { tunnels :: Set.Set Point,
                             items :: Map.Map Point Char,
                             curPos :: Point } deriving Show

initCaveState :: CaveState
initCaveState = CaveState { tunnels=Set.empty,
                            items=Map.empty,
                            curPos=point 0 0 }

loadMapHelper :: ((Int,Int) -> Char) -> ((Int,Int)->(Int,Int)) -> (Int,Int) -> CaveState -> CaveState
loadMapHelper getter transformer p@(x1,y1) st
    | tile == '#' = st
    | tile == '@' = st { tunnels = Set.insert ((uncurry point) $ transformer p) init_tun,
                         curPos = ((uncurry point) $ transformer p) }
    | tile == '.' = st { tunnels = Set.insert ((uncurry point) $ transformer p) init_tun }
    | otherwise = st { tunnels = Set.insert ((uncurry point) $ transformer p) init_tun,
                       items = Map.insert ((uncurry point) $ transformer p) tile init_items }
    where tile = getter p
          init_tun = tunnels st
          init_items = items st

loadMapData :: [String] -> CaveState
loadMapData map_data =
    foldr (loadMapHelper get transform) initCaveState indices
    where h = length map_data
          w = length $ head map_data
          transform = \(x,y) -> (x,(h-y-1))
          indices = (\x y -> (x,y)) <$> [0..(w-1)] <*> [0..(h-1)]
          get = \(x,y) -> (map_data !! y) !! x

type MutMap = Map.Map Char (Map.Map Char Int)

buildMutualMapsHelper :: Set.Set Point -> Map.Map Point Char -> Char -> MutMap -> MutMap
buildMutualMapsHelper map its name acc =
    Map.insert name dtopmap acc
    where nits = Map.filter (\c -> c /= name) $ its
          orig = fst $ head $ Map.toList $ Map.filter (\c -> c == name) $ its
          dmap = distMap map orig
          dtopmap = Map.foldrWithKey (\p c acc1 -> Map.insert c (dmap Map.! p) acc1) Map.empty nits

buildMutualMaps :: CaveState -> MutMap
buildMutualMaps cs =
    foldr (buildMutualMapsHelper (tunnels cs) its) Map.empty it_names
    where its = Map.insert (curPos cs) '@' (items cs)
          it_names = Set.toList $ Map.foldr (\c acc -> Set.insert c acc) Set.empty (its)

getDist :: MutMap -> Char -> Char -> Int
getDist mm a b = (mm Map.! a) Map.! b

--data MapTree = MapTree { keys :: String, doorDeps :: Map.Map Char MapTree } deriving (Show)

data MapSegment = MapSegment { keys :: [Char], doors :: [Char], segmap :: Set.Set Point } deriving (Show, Eq)

type MazeMap = [MapSegment]

buildMapSegment :: CaveState -> Point -> MapSegment
buildMapSegment cs orig =
    MapSegment { keys=rkeys, doors=border_doors, segmap=Set.fromList (Map.keys dMap) }
    where doors = Map.filter (isUpper) (items cs)
          dMap = distMapBy (\p -> Map.notMember p doors) (tunnels cs) orig
          rkeys = Map.elems $ Map.filterWithKey (\p c -> Map.member p dMap) $ Map.filter (isLower) (items cs)
          border_doors = Map.elems $ Map.filterWithKey (\p d -> (not $ null $ filter (\p -> Map.member p dMap) $ neighbors p)) doors

isCovered :: [MapSegment] -> Point -> Bool
isCovered segments p =
    foldr (||) False $ map (\seg -> Set.member p (segmap seg)) segments

buildMapSegmentsImp :: CaveState -> ([MapSegment], Set.Set Point) -> ([MapSegment], Set.Set Point)
buildMapSegmentsImp cs (m,rem)
    | rem == Set.empty = (m,rem)
    | otherwise = (newm, rem2)
    where next = Set.elemAt 0 rem
          newseg = buildMapSegment cs next
          newm = (newseg:m)
          rem2 = Set.filter (not . isCovered newm) rem

buildMapSegments :: CaveState -> MazeMap
buildMapSegments cs =
    fst $ head $ dropWhile (\(_,rem) -> not $ Set.null rem) $ iterate (buildMapSegmentsImp cs) ([],init_tunnels)
    where doors = Map.filter (isUpper) (items cs) -- All door items
          init_tunnels = Set.filter (\p -> Map.notMember p doors) (tunnels cs) -- All tunnels not under a door.

rootSegment :: CaveState -> MazeMap -> Maybe MapSegment
rootSegment cs segs
    | null state = Nothing
    | otherwise = Just (head state)
    where orig = (curPos cs)
          state = filter (\seg -> Set.member orig (segmap seg)) segs

data MazeSearchState = MazeSearchState { master_map :: MutMap, all_segs :: MazeMap, acquired_keys :: Set.Set Char, reachable_segs :: MazeMap } deriving (Show)

initMazeSearchState :: CaveState -> MazeSearchState
initMazeSearchState cs =
    MazeSearchState { master_map=mas_map, all_segs=all_seg, acquired_keys=Set.empty, reachable_segs=[init_seg] }
    where all_seg = buildMapSegments cs
          mas_map = buildMutualMaps cs
          init_seg = fromJust $ rootSegment cs all_seg

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let map_data = lines file_data
        cave_state = loadMapData map_data
        maze_map = buildMapSegments cave_state
        init_seg = fromJust $ rootSegment cave_state maze_map
    mapM_ (putStrLn) map_data
    --print $ cave_state
    --print $ distMap (tunnels cave_state) (curPos cave_state)
    --print $ distMapBy (\p -> Map.notMember p doors) (tunnels cave_state) (curPos cave_state)
    --print $ buildMutualMaps cave_state
    --print $ findBorderDoorsRkeys cave_state (curPos cave_state) ""
    --print $ buildMapSegment cave_state (curPos cave_state)
    --print $ buildMapSegments cave_state
    --print $ rootSegment cave_state $ buildMapSegments cave_state
    print $ initMazeSearchState cave_state
    --putStrLn $ "Task 1: " ++ (show $ shortestCollectDist cave_state)

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
    let progArgCont = AP.buildProgramArguments "day_18 solves Advent of Code 2019 day 18" argDefinitions
    case progArgCont of
        Left msg -> putStrLn msg
        Right progArgs -> argHelpHandler progArgs args
