import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Library.ArgParser as AP
import Data.Char
import Data.List

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Point = Point { gX :: Int, gY :: Int } deriving (Eq, Ord)

instance Show Point where
    show p = "{" ++ (show $ gX p) ++ "," ++ (show $ gY p) ++ "}"

point :: Int -> Int -> Point
point x y = Point { gX=x, gY=y }

add :: Point -> Point -> Point
add a b = Point { gX=(a_x+b_x), gY=(a_y+b_y) }
    where a_x = gX a
          b_x = gX b
          a_y = gY a
          b_y = gY b

neighbors :: Point -> [Point]
neighbors p = [ p {gY=(p_y+1)},
                p {gY=(p_y-1)},
                p {gX=(p_x+1)},
                p {gX=(p_x-1)} ]
    where p_x = gX p
          p_y = gY p

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

canExplore :: CaveState -> Point -> Bool
canExplore st p
    | Set.notMember p tun = False
    | not $ (p `elem` (Map.keys it)) = True
    | isUpper (it Map.! p) = False
    | otherwise = True
    where tun = tunnels st
          it = items st

distMapIteration :: (Point -> Bool) -> (Map.Map Point Int, [Point]) -> (Map.Map Point Int, [Point])
distMapIteration validator (dm, que) =
    (new_dm, q1 ++ next_ns)
    where q = head $ que
          q1 = drop 1 que
          from = head $ sortBy (\p1 p2 -> compare (dm Map.! p1) (dm Map.! p2)) $ filter (\p -> p `elem` (Map.keys dm)) $ filter (validator) $ neighbors q
          from_d = dm Map.! from
          new_dm = Map.insert q (from_d+1) dm
          next_ns = filter (\p -> not $ p `elem` q1) $ filter (\p -> not $ (p `elem` (Map.keys new_dm))) $ filter (validator) $ neighbors q

buildDistMap :: CaveState -> Map.Map Point Int
buildDistMap st =
    fst $ head $ dropWhile (\s -> not $ null $ snd s) $ iterate (distMapIteration (canExplore st)) (dm, queue)
    where tun = tunnels st
          cP = curPos st
          dm = Map.singleton cP 0
          queue = filter (canExplore st) $ neighbors cP

shortestCollectDist :: CaveState -> Int
shortestCollectDist st
    | Map.size keys_to_collect == 0 = 0
--    | Map.size keys_to_collect == 0 = []
    | otherwise = minimum $ foldr (\(pos,keyname) acc-> ((distMap Map.! pos)+(shortestCollectDist st { curPos=pos, items=Map.filter (\c -> (toLower c) /= keyname) it})):acc) [] reachable_keys
--    | otherwise = foldr (\(pos,keyname) acc-> ((distMap Map.! pos),st { curPos=pos, items=Map.filter (\c -> (toLower c) /= keyname) it}):acc) [] reachable_keys
    where it = items st
          keys_to_collect = Map.filter (\c -> isLower c) it
          distMap = buildDistMap st
          reachable_keys = Map.toList (Map.filterWithKey (\p c -> p `elem` (Map.keys distMap)) keys_to_collect)

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let map_data = lines file_data
        cave_state = loadMapData map_data
    putStrLn $ "Task 1: " ++ (show $ shortestCollectDist cave_state)

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
