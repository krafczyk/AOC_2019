import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

isAffected :: Asm.IntcodeProgram -> (Int,Int) -> Bool
isAffected program (x,y)
    | Asm.isError(result) = False
    | length output /= 1 = False
    | head output == 1 = True
    | otherwise = False
    where result = Asm.runProgram [x,y] program
          final_state = let (Right x) = result in x
          output = Asm.getOutput final_state

task1 :: Asm.IntcodeProgram -> IO ()
task1 program =
    do
    let max = (50-1)
        indices = (\x y -> (x,y)) <$> [0..max] <*> [0..max]
        affected_indices = filter (\p -> isAffected program p) indices
    putStrLn $ "Task 1: " ++ (show $ length affected_indices)

image :: Int -> ((Int,Int)->Char) -> [String]
image max imageFunc =
    foldr (\y_idx acc -> (foldr (\x_idx acc1 -> (imageFunc (x_idx,y_idx)):acc1) [] indices):acc) [] indices
    where indices = [0..max]

tractorImageFunc :: Asm.IntcodeProgram -> (Int,Int) -> Char
tractorImageFunc program p
    | isAffected program p = '#'
    | otherwise = '.'

binarySearchBool :: (Int -> Bool) -> Int -> Int -> Int
binarySearchBool valFunc small large
    | (small+1 == large) = if smallVal then small else large
    | smallVal = if midVal then binarySearchBool (valFunc) mid large else binarySearchBool (valFunc) small mid
    | otherwise = if midVal then binarySearchBool (valFunc) small mid else binarySearchBool (valFunc) mid large
    where smallVal = valFunc small
          largeVal = valFunc large
          mid = (large+small) `div` 2
          midVal = valFunc mid

rowEdges :: Asm.IntcodeProgram -> Int -> (Int,Int)
rowEdges program y =
    (x_small_edge, x_large_edge)
    where valFunc = \x -> isAffected program (x,y)
          x_mid = (5*y) `div` 4
          x_large = 3*x_mid
          x_large_edge = binarySearchBool (valFunc) x_mid x_large
          x_small = x_mid `div` 3
          x_small_edge = binarySearchBool (valFunc) x_small x_mid

rowWidth :: Asm.IntcodeProgram -> Int -> Int
rowWidth program y
    | y == 0 = 1
    | y > 0 && y < 3 = 0
    | y >= 3 && y <= 5 = 1
    | otherwise = large - small + 1
    where (small,large) = rowEdges program y

colEdges :: Asm.IntcodeProgram -> Int -> (Int,Int)
colEdges program x =
    (y_small_edge, y_large_edge)
    where valFunc = \y -> isAffected program (x,y)
          y_mid = (4*x) `div` 5
          y_large = 3*y_mid
          y_large_edge = binarySearchBool (valFunc) y_mid y_large
          y_small = y_mid `div` 3
          y_small_edge = binarySearchBool (valFunc) y_small y_mid

colHeight :: Asm.IntcodeProgram -> Int -> Int
colHeight program x =
    large - small + 1
    where (small, large) = colEdges program x

rowSatisfies :: Asm.IntcodeProgram -> Int -> Int -> Bool
rowSatisfies program size y
    | row_width < size = False
    | (ly-y+1) >= size = True
    | otherwise = False
    where row_width = rowWidth program y
          (small, large) = rowEdges program y
          h = (large-(size-1))
          (_,ly) = colEdges program h

fullSearch :: Asm.IntcodeProgram -> Int -> (Int,Int)
fullSearch program size =
    (fs_x, fs_y)
    where fs_y = binarySearchBool (rowSatisfies program size) (size `div` 2) (size*10)
          (x_low, x_high) = rowEdges program fs_y
          horizFunc = \x -> let (_, y_high) = colEdges program x in (y_high-fs_y+1) >= size
          fs_x = binarySearchBool (horizFunc) x_low x_high

task2 :: Asm.IntcodeProgram -> IO ()
task2 program =
    do
    let (a_x, a_y) = fullSearch program 100
    putStrLn $ "Task 2: " ++ (show $ (a_x*10000)+a_y)

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle =
    do
    file_data <- hGetContents handle
    let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
    task1 program
    task2 program

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
    let progArgCont = AP.buildProgramArguments "Day template" argDefinitions
    case progArgCont of
        Left msg -> putStrLn msg
        Right progArgs -> argHelpHandler progArgs args
