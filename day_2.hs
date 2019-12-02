import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.List.Split

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

type Inst3 = Int -> Int -> Int -> [Int] -> [Int]
type IntcodeState = (Int, [Int])

applyInst3 :: Inst3 -> Int -> Int -> Int -> [Int] -> Maybe [Int]
applyInst3 func a b c state
    | a >= state_length = Nothing
    | b >= state_length = Nothing
    | c >= state_length = Nothing
    | otherwise = Just (func a b c state)
    where state_length = length state


addInst :: Inst3
addInst a b c state = replaceNth c ((state !! a)+(state !! b)) state

multInst :: Inst3
multInst a b c state = replaceNth c ((state !! a)*(state !! b)) state

advanceStateFunc3 :: Inst3 -> IntcodeState -> Maybe [Int]
advanceStateFunc3 func (idx, state) = applyInst3 func (state !! (idx+1)) (state !! (idx+2)) (state !! (idx+3)) state

advanceState :: IntcodeState -> Maybe IntcodeState
advanceState (idx, state)
    | (state !! idx) == 1 = let nextState = advanceStateFunc3 addInst (idx,state) in
                            case nextState of
                                Nothing -> Nothing
                                Just nState -> Just (idx+4, nState)
    | (state !! idx) == 2 = let nextState = advanceStateFunc3 multInst (idx,state) in
                            case nextState of
                                Nothing -> Nothing
                                Just nState -> Just (idx+4, nState)
    | (state !! idx) == 99 = Just (idx, state) -- This is the exit condition
    | otherwise = Nothing

advanceCondition :: Maybe IntcodeState -> Bool
advanceCondition Nothing = False
advanceCondition (Just (idx, state)) = if (state !! idx) == 99 then False else True

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

fileHandler handle = do
                     file_data <- hGetContents handle
                     let intcode_prog = map (\x -> read x :: Int) $ splitOn "," (lines file_data !! 0)
                     --putStrLn $ show $ takeWhileInclusive advanceCondition $ iterate (>>=advanceState) (Just (0, intcode_prog)) --  This debug section is great for testing the program engine works properly.
                     -- We modify the initial program.
                     let task_1_prog = replaceNth 1 12 $ replaceNth 2 2 intcode_prog
                         final_state = (take 1 $ reverse $ takeWhileInclusive advanceCondition $ iterate (>>=advanceState) (Just (0, task_1_prog))) !! 0
                     case final_state of
                         Nothing -> putStrLn "Task 1 failed!"
                         Just (idx, state) -> putStrLn $ "Task 1: " ++ show (state !! 0)

handleFile :: String -> IO ()
handleFile input_filepath = withFile input_filepath ReadMode fileHandler

argHelpHandler progArgs args = if AP.helpPresent progArgs args
                                   then AP.writeHelpText progArgs
                                   else let parse_result = AP.parseArguments progArgs args in
                                       case parse_result of
                                            Left msg -> putStrLn msg
                                            Right argMap -> case Map.lookup "input_filepath" argMap of
                                                                Just x -> let input_filepath = (reverse x !! 0) !! 0 in
                                                                          handleFile input_filepath
                                                                Nothing -> putStrLn "Couldn't get input_filepath"

main = do
       args <- getArgs
       let progArgCont = AP.buildProgramArguments "day_2 solves Advent of Code 2019 day 2" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
