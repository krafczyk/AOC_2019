import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import Data.List
import Data.List.Split
import Data.Char

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

data Vector = Vector { gX :: Int, gY :: Int, gZ :: Int } deriving (Eq)

instance Show Vector where
    show p = (show $ gX p) ++ "," ++ (show $ gY p) ++ "," ++ (show $ gZ p)

instance Ord Vector where
    compare a b
        | c_x /= EQ = c_x
        | c_y /= EQ = c_y
        | otherwise = compare a_z b_z
        where a_x = gX a
              b_x = gX b
              c_x = compare a_x b_x
              a_y = gY a
              b_y = gY b
              c_y = compare a_y b_y
              a_z = gZ a
              b_z = gZ b

instance Read Vector where
    readsPrec _ value
        | mbegin_idx == Nothing = []
        | mend_idx == Nothing = []
        | length split_1 /= 3 = []
        | map (length) split_2 /= [2,2,2] = []
        | map (head) split_2 /= ["x", "y", "z"] = []
        | (length $ filter (\l -> not $ null l) v_parses) /= 3 = []
        | otherwise = [ (Vector { gX=x, gY=y, gZ=z }, rest) ]
        -- We attempt to progressively read the input value.
        where mbegin_idx = findIndex (=='<') value
              mend_idx = findIndex (=='>') value
              begin_idx = let (Just bidx) = mbegin_idx in bidx
              end_idx = let (Just eidx) = mend_idx in eidx
              selected = filter (not . isSpace) $ take (end_idx-(begin_idx+1)) $ drop (begin_idx+1) value
              rest = drop (end_idx+1) value
              split_1 = splitOn "," selected
              split_2 = map (splitOn "=") split_1
              nums = map (head . (drop 1)) split_2
              v_parses = map (\x -> reads x :: [(Int,String)]) nums
              [x,y,z] = map (\v -> read v :: Int) nums

vec1 :: Int -> Int -> Int -> Vector
vec1 x y z = Vector { gX=x, gY=y, gZ=z }

vec2 :: (Int, Int, Int) -> Vector
vec2 (x,y,z) = vec1 x y z

add :: Vector -> Vector -> Vector
add a b = Vector { gX=(a_x+b_x), gY=(a_y+b_y), gZ=(a_z+b_z) }
    where a_x = gX a
          b_x = gX b
          a_y = gY a
          b_y = gY b
          a_z = gZ a
          b_z = gZ b

data Planet = Planet { pos :: Vector, vel :: Vector } deriving (Eq,Show,Ord)

type Planets = [(Int,Planet)]

initPlanet :: Vector -> Planet
initPlanet init_pos = Planet { pos=init_pos, vel=vec1 0 0 0 }

gravDiff1 :: Int -> Int -> Int
gravDiff1 a b
    | a < b = 1
    | a > b = -1
    | otherwise = 0

gravDiff :: Vector -> Vector -> Vector
gravDiff a b =
    Vector { gX=(gravDiff1 (gX a) (gX b)),
             gY=(gravDiff1 (gY a) (gY b)),
             gZ=(gravDiff1 (gZ a) (gZ b)) }

apGF2 :: Planet -> (Int, Planet) -> Vector -> Vector
apGF2 p1 (_, p2) acc = add acc (gravDiff (pos p1) (pos p2))

apGF1 :: Planets -> (Int, Planet) -> Planets -> Planets
apGF1 planets (idx1, p1) acc = (idx1, p1 {vel=new_vel} ):acc
    where vel_diff = foldr (apGF2 p1) (vec1 0 0 0) filt_planets 
          filt_planets = filter (\(idx,_) -> idx /= idx1) planets
          vp1 = vel p1
          new_vel = add vp1 vel_diff

applyGravity :: Planets -> Planets
applyGravity planets = foldr (apGF1 planets) [] planets

applyVelocity :: Planets -> Planets
applyVelocity planets =
    foldr (\(idx, p) acc -> (idx, p {pos=(add (pos p) (vel p)) }):acc) [] planets

applyTimeStep :: Planets -> Planets
applyTimeStep planets = applyVelocity $ applyGravity planets

mag :: Vector -> Int
mag v = (abs $ gX v)+(abs $ gY v)+(abs $ gZ v)

energy :: Planet -> Int
energy p = (mag $ pos p)*(mag $ vel p)

applyTimeStep2 :: (Planets,Set.Set Planets) -> (Planets,Set.Set Planets)
applyTimeStep2 (cur, past) = (next, Set.insert cur past)
    where next = applyTimeStep cur

isRepetition :: (Planets,Set.Set Planets) -> Bool
isRepetition (cur, past) = Set.member cur past

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let planet_lines = lines file_data
                                planet_ps = map (\l -> read l :: Vector) planet_lines
                                planets = zip [0..] (map (\p -> initPlanet p) planet_ps)
                                step = 1000
                                step_result = head $ drop step $ iterate (applyTimeStep) planets
                                total_energy = sum $ map (energy) $ map (snd) $ step_result
                            putStrLn $ "Task 1: " ++ (show total_energy)
                            let rep_state = head $ dropWhile (not . isRepetition) $ iterate (applyTimeStep2) (planets, Set.empty)
                            putStrLn $ "Task 2: " ++ (show $ Set.size $ snd rep_state)

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
       let progArgCont = AP.buildProgramArguments "day_12 Solves Advent of Code 2019 day 12" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
