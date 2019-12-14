import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Utility as U
import Data.List.Split
import Data.Char
import Data.List

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]


-- 7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
-- 171 ORE => 8 CNZTR

trimWhitespace :: String -> String
trimWhitespace input = reverse $ dropWhile (isSpace) $ reverse $ dropWhile (isSpace) input

buildPair :: [String] -> Maybe (String, Int)
buildPair ins
    | length ins /= 2 = Nothing
    | otherwise = Just (y, read x :: Int)
    where [x,y] = ins

line_digest :: String -> Either String ((String, Int),[(String,Int)])
line_digest input_line
    | length outputs_2 /= 2 = Left $ "Out error 1: " ++ (show $ length outputs_2)
    | otherwise =
    case inputs_3 of
        Nothing -> Left "Out error 2"
        Just ins -> Right $ ((o_name, o_n),ins)
    where first_split = map (trimWhitespace) $ splitOn "=>" input_line
          inputs_1 = head first_split
          inputs_2 = map (trimWhitespace) $ splitOn "," inputs_1
          inputs_3 = sequence $ map (buildPair) $ map (splitOn " ") $ inputs_2
          outputs_1 = head $ drop 1 first_split
          outputs_2 = splitOn " " outputs_1
          outputs_3 = buildPair outputs_2
          (o_name, o_n) = let (Just o3) = outputs_3 in o3

type RecipeBook = Map.Map String (Int,Map.Map String Int)

buildRecipeBook :: [((String, Int),[(String,Int)])] -> RecipeBook
buildRecipeBook digested = foldr (\((name, n),outs) acc -> Map.insert name (n,Map.fromList outs) acc) Map.empty digested

hasDependency :: RecipeBook -> [String] -> String -> Bool
hasDependency recipe_book decendents parent
    | decendents == [] = False
    | mcmap == Nothing = False
    | length children == 0 = False
    | not . null $ dropWhile (\x -> not $ x `elem` decendents) children = True
    | null filt_chil = False
    | otherwise = True
    where mcmap = Map.lookup parent recipe_book
          children = let Just (_,cmap) = mcmap in (filter (\x -> Map.member x recipe_book) (Map.keys cmap))
          filt_chil = dropWhile (not . hasDependency recipe_book decendents) children

expandStepHelper :: RecipeBook -> Map.Map String Int -> String -> Map.Map String Int
expandStepHelper recipe_book init_state selected = Map.map (*num_need) result
    where num_sel = init_state Map.! selected
          (num_prod, result) = recipe_book Map.! selected
          num_need = if (num_sel `mod` num_prod) == 0 then num_sel `div` num_prod else (num_sel `div` num_prod)+1

expandStep :: RecipeBook -> Map.Map String Int -> Map.Map String Int
expandStep recipe_book init_state
    | null to_expand = init_state
    | null with_deps = Map.delete no_dep (Map.unionWith (+) init_state (expandStepHelper recipe_book init_state no_dep))
    | otherwise = Map.delete dep (Map.unionWith (+) init_state (expandStepHelper recipe_book init_state dep))
    where to_expand_1 = Map.keys init_state
          to_expand = filter (\x -> Map.member x recipe_book) to_expand_1
          with_deps = filter (\x -> hasDependency recipe_book (delete x to_expand) x) to_expand
          dep = head $ with_deps
          without_deps = foldr (\x acc -> if x `elem` with_deps then acc else x:acc) [] to_expand
          no_dep = head $ without_deps

solveProblems :: RecipeBook -> IO ()
solveProblems recipe_book =
    do
    let start = Map.singleton "FUEL" 1
        steps = U.takeWhileInclusive (\m -> Map.keys m /= ["ORE"]) $ iterate (expandStep recipe_book) start
    mapM_ (putStrLn . show) steps
    --let to_expand = filter (\x -> Map.member x recipe_book) $ Map.keys step
    --print $ to_expand
    --let with_deps = filter (\x -> hasDependency recipe_book (delete x to_expand) x) to_expand
    --print $ with_deps

indexMap :: [((String, Int),[(String,Int)])] -> IO ()
indexMap digested =
    do
    let recipe_book = buildRecipeBook digested
    solveProblems recipe_book

validateMap :: [((String, Int),[(String, Int)])] -> IO ()
validateMap digested =
    do
    -- Check there is only one unique product from each recipe
    let recipe_count :: Map.Map String Int
        recipe_count = foldr (\((name,_),_) acc -> Map.unionWith (+) (Map.singleton name 1) acc) Map.empty digested
        max_recipes = Map.foldr (max) 0 recipe_count
    if max_recipes /= 1
        then putStrLn $ "Map has multiple recipes producing the same item."
        else indexMap digested

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let digested = sequence $ map (line_digest) $ lines file_data
                            case digested of
                                Left msg -> putStrLn $ "Couldn't load data: " ++ msg
                                Right dig -> validateMap dig

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
       let progArgCont = AP.buildProgramArguments "day_14 solves Advent of code 2019 day 14" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
