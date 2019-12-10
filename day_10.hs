import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import Data.List

data Point = Point { getX :: Int, getY :: Int } deriving (Eq)

showNice :: Point -> String
showNice a = (show x) ++ "," ++ (show y)
    where x = getX a
          y = getY a

instance Show Point where
    show a = "p=("++(show x) ++ "," ++ (show y) ++ ")"
        where x = getX a
              y = getY a

quadDecompose :: Point -> (Int, Int, Int)
quadDecompose p
    | x >= 0 && y > 0 = (1, x, y)
    | x > 0 && y <= 0 = (2, -y, x)
    | x <= 0 && y < 0 = (3, -x, -y)
    | otherwise = (4, y, -x)
    where x = getX p
          y = getY p

instance Ord Point where
    compare a b
        | a_q /= b_q = compare a_q b_q
        | a_x == 0 && b_x == 0 = compare a_x b_x 
        | a_x == 0 = LT
        | b_x == 0 = GT
        | otherwise = compare ((toRational a_x)/(toRational a_y)) ((toRational b_x)/(toRational b_y))
        where (a_q, a_x, a_y) = quadDecompose a
              (b_q, b_x, b_y) = quadDecompose b

pDiff :: Point -> Point -> Point
-- getY is flipped because the coordinate system grows down with increasing y coordinate
pDiff a b = Point { getX=(getX b-getX a), getY=(getY a-getY b) }

add :: Point -> Point -> Point
add a b = Point { getX=(getX a+getX b), getY=(getY a+getY b) }

reduceDiff :: Point -> Point
reduceDiff in_p
    | x == 0 && y == 0 = in_p
    | x == 0 = Point { getX=0, getY=(y `div` yy) }
    | y == 0 = Point { getX=(x `div` xx), getY=0 }
    | otherwise = Point { getX=((x*xxx) `div` xx), getY=((y*yyy) `div` yy) }
    where x = getX in_p
          y = getY in_p
          xx = abs x
          yy = abs y
          the_gcd = gcd xx yy
          xxx = xx `div` the_gcd
          yyy = yy `div` the_gcd

buildAsteroidList :: [String] -> [Point]
buildAsteroidList asteroid_map =
    foldl (\acc (line, y_idx) ->
        let line_asteroids = foldl (\acc (c, x_idx) -> if c == '#' then acc ++ [Point { getX=x_idx, getY=y_idx}] else acc) [] (zip line [0..]) in
        if line_asteroids == []
            then acc
            else acc ++ line_asteroids) [] line_enum
    where line_enum = zip asteroid_map [0..]

othersVisible :: [Point] -> [(Point,Int)]
othersVisible points =
    foldr (\test_p acc ->
        let abbrev_list = delete test_p points
            diff_list = map (reduceDiff) $ foldr (\a_p acc -> (pDiff test_p a_p):acc) [] abbrev_list in
        (test_p, length $ nub diff_list):acc ) [] points

maxVisible :: [(Point,Int)] -> (Point, Int)
maxVisible list = foldr1 (\(a_p, n) (m_p, m) -> if n > m then (a_p, n) else (m_p, m)) list

type TargetMap = Map.Map Point [Point]

classifyTargets :: [Point] -> TargetMap
classifyTargets points =
    foldr (\p acc -> let red_p = reduceDiff p in
                     Map.unionWith (++) (Map.singleton red_p [p]) acc) Map.empty points

sortByDist :: [Point] -> [Point]
sortByDist points = sortBy (\a b -> compare (distFunc a) (distFunc b)) points
    where distFunc = \p -> (abs $ getX p) + (abs $ getY p)

vaporizeTarget :: ([Point], TargetMap) -> ([Point], TargetMap)
-- No targets yet vaporized.
vaporizeTarget ([], m) = ([target], new_map)
    where key = head $ Map.keys m
          sorted_list = sortByDist $ m Map.! key
          target = head sorted_list
          new_list = drop 1 sorted_list
          new_map = if length new_list == 0 then Map.delete key m else Map.insert key new_list m
vaporizeTarget (ts, m) = (target:ts, new_map)
    where last_target_key = reduceDiff $ head ts
          key_list = Map.keys m
          init_key_list = dropWhile (<=last_target_key) key_list
          key = if init_key_list == [] then head key_list else head init_key_list
          sorted_list = sortByDist $ m Map.! key
          target = head sorted_list
          new_list = drop 1 sorted_list
          new_map = if length new_list == 0 then Map.delete key m else Map.insert key new_list m

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let asteroid_map = lines file_data
                            let asteroid_list = buildAsteroidList asteroid_map
                                visibility_list = othersVisible asteroid_list
                                max_vis = maxVisible visibility_list
                            putStrLn $ "Task 1: Best is " ++ (showNice $ fst max_vis) ++ " with " ++ (show $ snd max_vis) ++ " detected"
                            let station_location = fst max_vis
                            putStrLn $ "The station will be installed at " ++ (showNice station_location)
                            let abbrev_list = delete station_location asteroid_list
                                init_diff_list = foldr (\a_p acc -> (pDiff station_location a_p):acc) [] abbrev_list
                            let target_map = classifyTargets init_diff_list
                                simulation = U.takeWhileInclusive (\(_, map) -> map /= Map.empty) $ iterate (vaporizeTarget) ([], target_map)
                                kill_order = reverse $ fst $ last $ simulation
                                kill_order_transform = map (\p -> add station_location p {getY= -(getY p)}) kill_order
                                kill_of_interest = kill_order_transform !! 199
                            putStrLn $ "The 200th asteroid to be vaporized is " ++ (showNice $ kill_of_interest)
                            putStrLn $ "Task 2: " ++ (show $ (getX kill_of_interest)*100+getY kill_of_interest)

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
       let progArgCont = AP.buildProgramArguments "day_10 solves Advent of Code 2019 Day 10" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
