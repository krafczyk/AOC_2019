import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.List

data Point = Point { getX :: Int, getY :: Int } deriving (Show, Eq, Ord)

--sieve :: [Int] -> [Int]
--sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /= 0]
--
--genPrimes :: Int -> [Int]
--genPrimes max = takeWhile (<max) $ 2:sieve [3,5..]
--
--type Factors = Map.Map Int Int
--
--factorImp :: [Int] -> Int -> Factors
--factorImp _ 1 = Map.empty
--factorImp fs x = Map.unionWith (+) (Map.singleton f 1) (factorImp fs (x `div` f))
--    where f = head $ take 1 [f' | f' <- fs, x `mod` f == 0]
--
--factor :: Int -> Factors
--factor x = factorImp primes k
--    where primes = genPrimes (x+1)
--
--buildFromFactors :: Factors -> Int
--buildFromFactors factors = Map.foldrWithKey (\k a acc -> acc*(a^k)) 1 factors
--
--gcd :: Int -> Int -> Int
--gcd A B = buildFromFactors Common_factors
--    where A_factors = factor A
--          B_factors = factor B
--          Common_factors = M.intersectionWith (min) A_factors B_factors

pDiff :: Point -> Point -> Point
pDiff a b = Point { getX=(getX b-getX a), getY=(getY b-getY a) }

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

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let asteroid_map = lines file_data
                            mapM_ putStrLn asteroid_map -- Read problem data here
                            let asteroid_list = buildAsteroidList asteroid_map
                                visibility_list = othersVisible asteroid_list
                                max_vis = maxVisible visibility_list
                            putStrLn $ "Task 1: Best is " ++ (show $ getX $ fst max_vis) ++ "," ++ (show $ getY $ fst max_vis) ++ " with " ++ (show $ snd max_vis) ++ " detected"

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
