import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Library.ArgParser as AP
import Library.Vec
import Library.Maze

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

data CaveState = CaveState { tunnels :: Set.Set Point, portalsA :: Map.Map Char (Point,Point), portalsB :: Map.Map Point Char} deriving Show

initCaveState :: CaveState
initCaveState = CaveState { tunnels=Set.empty, portalsA=Map.empty, portalsB=Map.empty }

loadMapHelper :: ((Int,Int) -> Char) -> ((Int,Int)->(Int,Int)) -> (Int,Int) -> CaveState -> CaveState
loadMapHelper getter transformer p@(x1,y1) st
    | tile == '.' = st { tunnels = Set.insert ((uncurry point) $ transformer p) init_tun }
    | otherwise = st
    where tile = getter p
          init_tun = tunnels st

loadMapData :: [String] -> CaveState
loadMapData map_data =
    where h = length map_data
          w = length $ head map_data
          transform = \(x,y) -> (x,(h-y-1))
          indices = (\x y -> (x,y)) <$> [0..(w-1)] <*> [0..(h-1)]
          get = \(x,y) -> (map_data !! y) !! x
          tunnels = foldr (loadMapHelper get transform) initCaveState indices
		  ns = \(x,y) -> [(x,y-1),(x+1,y),(x,y+1),(x-1,y)]
          ulab = \(x,y) -> [get (x,y-2),get (x,y-1)]
          rlab = \(x,y) -> [get (x+1,y),get (x+2,y)]
          llab = \(x,y) -> [get (x-2,y),get (x-1,y)]
3~0
          dlab = \(x,y) -> [get (x,y+1),get (x,y+2)]
          glab :: Map.Map Int ((Int,Int) -> String)
          --glab = Map.fromList -- TODO RIGHT HERE. Trying to write functions to automatically extract location labels.
          getLabel = \p -> let ls = filter (\(n,p2) -> isUpper $ get p2)) $ zip [0..] (ns p) in if null ls then "" else  

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let map_data = lines file_data
        cs = loadMapData map_data
    mapM_ (putStrLn) $ map_data
    print $ cs

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
    let progArgCont = AP.buildProgramArguments "day_20 solves Advent of Code 2019 day 20" argDefinitions
    case progArgCont of
        Left msg -> putStrLn msg
        Right progArgs -> argHelpHandler progArgs args0
