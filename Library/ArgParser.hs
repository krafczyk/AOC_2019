module Library.ArgParser where

import Data.List
import qualified Data.Map as Map

type ArgName = String
type ArgHandles = [String]
type ArgHelptext = String
type EasyArgDefinitions = (String, [String], String, Int)
type ArgMap = Map.Map String [[String]]
data ArgDefinitions = ArgDefinitions { name :: ArgName, handles :: ArgHandles, helpText :: ArgHelptext, numArgs :: Int } deriving (Show)
data ProgramArguments = ProgramArguments { progTitle :: String, argDefs :: [ArgDefinitions] } deriving (Show)

helpArgDef :: ArgDefinitions
helpArgDef = ArgDefinitions { name="help",
                              handles=["-h", "--help"],
                              helpText="Print this help text",
                              numArgs=0 }

buildProgArgFunc :: EasyArgDefinitions -> ArgDefinitions
buildProgArgFunc (name, handles, helptext, num) = ArgDefinitions { name=name,
                                                                   handles=handles,
                                                                   helpText=helptext,
                                                                   numArgs=num }

checkForRepeated :: [String] -> Maybe String
checkForRepeated list = let (test, val, _) = foldl (\(test, val, accl) x ->
                                                        if test then
                                                            (test, val, accl)
                                                        else
                                                            if x `elem` accl then
                                                                (True, x, x:accl)
                                                            else
                                                                (False, "", x:accl)) (False, "", []) list in
                        if test then
                            Just val
                        else
                            Nothing

argNameConflicts :: [EasyArgDefinitions] -> Maybe String
argNameConflicts easyargs = let arg_names = map (\(n, _, _, _) -> n) easyargs in
                            checkForRepeated arg_names

argHandleConflicts :: [EasyArgDefinitions] -> Maybe String
argHandleConflicts easyargs = let handle_names = foldr (\x acc -> x++acc) [] $ map (\(_, h, _, _) -> h) easyargs in
                              checkForRepeated handle_names

buildProgramArguments :: String -> [EasyArgDefinitions] -> Either String ProgramArguments
buildProgramArguments prog_title user_easyargs = case (argNameConflicts user_easyargs) of
                                                     Nothing -> case (argHandleConflicts user_easyargs) of
                                                                Nothing -> let user_argdefs = map buildProgArgFunc user_easyargs in
                                                                           Right ProgramArguments { progTitle=prog_title,
                                                                                                    argDefs=helpArgDef:user_argdefs }
                                                                Just x -> Left $ "ArgParser ERROR: Argument handle " ++ x ++ " conflicts."
                                                     Just x -> Left $ "ArgParser ERROR: Argument name " ++ x ++ " conflicts."

printArgDef :: ArgDefinitions -> IO ()
printArgDef argdef = let rep_str = intercalate "/" (handles argdef)
                         name_str = name argdef
                         help_str = helpText argdef in
                     putStrLn $ rep_str ++ ": stored as " ++ name_str ++ " = " ++ help_str

writeHelpText :: ProgramArguments -> IO ()
writeHelpText progArgs = do
                         putStrLn $ progTitle progArgs
                         putStrLn $ "Accepts arguments"
                         mapM_ (printArgDef) $ argDefs progArgs

getNamePositions :: [String] -> [String] -> [Int]
getNamePositions names args = map fst $ filter (\(_,n) -> n `elem` names) $ zip [0..] args

helpPresent :: ProgramArguments -> [String] -> Bool
helpPresent progArgs args = let help_handles = foldl (++) [] $ map (\x -> handles x) $ filter (\x -> name x == "help") $ argDefs progArgs in
                            not $ null $ getNamePositions help_handles args

getArgByName :: ProgramArguments -> String -> ArgDefinitions
getArgByName progArgs argname = (filter (\x -> name x == argname) $ argDefs progArgs) !! 0

doArgsSupport :: ArgDefinitions -> Int -> [String] -> Bool
doArgsSupport argDef idx args = let num_req = numArgs argDef in
                                if length (take num_req (drop (idx+1) args)) == num_req
                                    then True
                                    else False

parseArguments :: ProgramArguments -> [String] -> Either String ArgMap
parseArguments progArgs args = let name_handle_pairs = map (\x -> (name x, handles x)) $ filter (\x -> name x /= "help") $ argDefs progArgs
                                   name_idx_pairs = map (\(name, handles) -> (name, getNamePositions handles args)) name_handle_pairs
                                   support_check_a = map (\(name, idxs) -> (name, map (\idx -> doArgsSupport (getArgByName progArgs name) idx args) idxs )) name_idx_pairs
                                   support_check = map (\(name, support) -> name) $ filter (\(name, support) -> if not $ foldl (&&) True support then True else False ) $ support_check_a in
                               if not . null $ support_check
                                   then Left $ "ArgParser ERROR: argument names " ++ (intercalate "," support_check) ++ " not given enough arguments!"
                                   else let support_check_2 = map (\(n,x) -> n) $ filter (\(n, x) -> not x) $ map (\(n,x) -> (n, foldl (&&) True $ map (>0) x)) $ filter (\(n,x) -> not . null $ x) $ map (\(n,x) -> (n, map (\(a,b) -> a-b-(numArgs $ getArgByName progArgs n)) $ zip (drop 1 x) (reverse . drop 1 . reverse $ x))) name_idx_pairs in
                                       if not . null $ support_check_2
                                           then Left $ "ArgParser ERROR: argument names " ++ (intercalate "," support_check_2) ++ " not enough space between arguments!"
                                           else let map_build_data = filter (\(n, items) -> not . null $ items) $ map (\(n, idxs) -> let num_args = numArgs $ getArgByName progArgs n in (n, map (\idx -> map (args !!) $ map (idx+) $ take num_args [1..]) idxs)) name_idx_pairs in
                                                Right $ Map.fromList map_build_data
