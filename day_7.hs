import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import qualified Library.Asm as Asm
import qualified Library.Utility as U
import Data.List.Split
import Data.List

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

type AmplifierProgram = (Int, [Asm.IntcodeState])

initAmplifierProgram :: Asm.IntcodeProgram -> [Int] -> Int -> AmplifierProgram
initAmplifierProgram program theSequ input = (input, ampProg)
    where ampProg = foldl (\acc x -> acc ++ [([x], [], 0, program)]) [] theSequ

runAmplifierStep :: (Int, Asm.IntcodeState) -> Asm.ComputeMonad Asm.IntcodeState
runAmplifierStep (inp, (inputs, outputs, cur_idx, prog_state)) = next_state
    where next_state = Asm.runProgramTillNeedInput (inputs ++ [inp], outputs, cur_idx, prog_state)

runAmplifierFold :: Asm.IntcodeState -> AmplifierProgram -> Asm.ComputeMonad AmplifierProgram
runAmplifierFold next_prog (inp, old_progs) =
    case next_state of
        Left msg -> Left msg
        Right (inputs, outputs, cur_idx, new_state) -> let outp = head outputs in
                                                       Right (outp, old_progs ++ [(inputs, drop 1 outputs, cur_idx, new_state)])
    where next_state = runAmplifierStep (inp, next_prog)

runAmplifierProgram :: AmplifierProgram -> Asm.ComputeMonad AmplifierProgram
runAmplifierProgram (input, programs) = step_result
    where step_result = foldl (\acc x -> acc>>=(runAmplifierFold x)) (Right (input, [])) programs

-- Easy programs for Task 1
runAmplifiers :: Asm.IntcodeProgram -> [Int] -> Int -> Asm.ComputeMonad Int
runAmplifiers program theSequ input =
    case compute_result of
        Left msg -> Left msg
        Right (outp, _) -> Right outp
    where init_amplifiers = initAmplifierProgram program theSequ input
          compute_result = runAmplifierProgram init_amplifiers

-- Need to define special runners for Task 2.
continueProg :: Asm.ComputeMonad AmplifierProgram -> Bool
continueProg c_amp_prog =
    case c_amp_prog of
        Left _ -> False
        Right (_, progs) -> let (_, _, idx, state) = last progs in
                            if (Asm.getOpcode (state !! idx)) == 99 then False else True

runAmplifiersFeedback :: Asm.IntcodeProgram -> [Int] -> Int -> Asm.ComputeMonad Int
runAmplifiersFeedback program theSequ input = 
    case last_state of
        Left msg -> Left msg
        Right (outp, _) -> Right outp
    where init_amplifiers = initAmplifierProgram program theSequ input
          apply_sequence = U.takeWhileInclusive (continueProg) $ iterate (>>=runAmplifierProgram) (Right init_amplifiers)
          last_state = last apply_sequence

fileHandler :: AP.ArgMap -> System.IO.Handle -> IO ()
fileHandler argMap handle = do
                            file_data <- hGetContents handle
                            let program = map (\x -> read x :: Int) $ splitOn "," $ head $ lines file_data
                                sequences = permutations [0,1,2,3,4]
                                task_1_results = sequence $ map (\theSequ -> runAmplifiers program theSequ 0) sequences
                            case task_1_results of
                                Left msg -> putStrLn $ "Task 1 failed with error " ++ (show msg)
                                Right vals -> putStrLn $ "Task 1: " ++ (show $ maximum vals)
                            let sequences = permutations [5,6,7,8,9]
                                task_2_results = sequence $ map (\theSequ -> runAmplifiersFeedback program theSequ 0) sequences
                            case task_2_results of
                                Left msg -> putStrLn $ "Task 2 failed with error " ++ (show msg)
                                Right vals -> putStrLn $ "Task 2: " ++ (show $ maximum vals)

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
       let progArgCont = AP.buildProgramArguments "Day template" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
