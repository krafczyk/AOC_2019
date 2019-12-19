module Library.Asm where

import qualified Library.Utility as U
import Control.Monad.Except
import qualified Data.Map as Map

-- Define the computation exception
data ComputeError = MemoryUnderrun Int -- Attempted to access memory before the start of memory
                  | UnsupportedOutputMode Int -- Attempted to use an unsupported output mode
                  | BuildBoundsErrorUnexpected Int -- buildBounds Error used with value within bounds
                  | UnsupportedInstruction Int 
                  | NoInputAvailable
                  | UnsupportedInputMode Int -- Attempted to use an unsupported Input mode.
                  | UnhandledOutputMode Int
                  | EarlyExit String
                  deriving (Eq)

instance Show ComputeError where
    show (MemoryUnderrun idx) = "Attempted to access memory before the start!: " ++ (show idx)
    show (UnsupportedOutputMode mode) = "Tried to use unsupported mode " ++ (show mode) ++ " with the output parameter"
    show (BuildBoundsErrorUnexpected idx) = "Used buildBoundsError with value within bounds! " ++ (show idx)
    show (UnsupportedInstruction opcode) = "Opcode " ++ (show opcode) ++ " is unsupported"
    show NoInputAvailable = "No input available"
    show (UnsupportedInputMode mode) = "Unsupported input mode " ++ (show mode)
    show (UnhandledOutputMode mode) = "Unhandled output mode " ++ (show mode)
    show (EarlyExit str) = "Exiting Early! (" ++ str ++ ")"

type ComputeMonad = Either ComputeError

-- The Intcode basic types
type IntcodeProgram = [Int]
type IntcodeInputs = [Int]
type IntcodeOutputs = [Int]
type IntcodePtr = Int

-- The intcode state definition
data IntcodeState = IntcodeState { getInput :: IntcodeInputs,
                                   getOutput :: IntcodeOutputs,
                                   getIdx :: IntcodePtr,
                                   getRelIdx :: IntcodePtr,
                                   getProgram :: IntcodeProgram } deriving (Show)

initState :: IntcodeProgram ->  IntcodeInputs -> IntcodeState
initState program inputs = IntcodeState { getInput=inputs,
                                          getOutput=[],
                                          getIdx=0,
                                          getRelIdx=0,
                                          getProgram=program }

-- The intcode opmode type
type IntcodeOpmode = Int

-- The intcode opcode type
type IntcodeOpcode = Int

-- The intcode mode type
type IntcodeMode = Int

-- Helper functions to get opcodes and modes
getOpcode :: IntcodeOpmode -> IntcodeOpcode
getOpcode opmode = opmode `mod` 100

getParMode :: IntcodeOpmode -> Int -> IntcodeMode
getParMode opmode i = ((opmode `div` 100) `div` (10^(i-1)))`mod` 10

numPars :: Map.Map Int (Int, Bool)
numPars = Map.fromList [(1, (2, True)),
                        (2, (2, True)),
                        (3, (0, True)),
                        (4, (1, False)),
                        (5, (2, False)),
                        (6, (2, False)),
                        (7, (2, True)),
                        (8, (2, True)),
                        (9, (1, False)),
                        (99, (0, False))]

-- Functions related to access of the program memory/state
buildBoundsError :: Int -> ComputeMonad a
buildBoundsError idx = throwError (MemoryUnderrun idx)

growMem :: Int -> [Int] -> [Int]
growMem idx program
    | idx >= prog_len = program ++ (take (idx-prog_len+1) $ repeat 0)
    | otherwise = program
    where prog_len = length program

-- opcode, input values, output info, program state
type PrepActionType = (Int, [Int], [(Int, Int)], IntcodeState)

prepActionImp :: Int -> Int -> PrepActionType -> ComputeMonad PrepActionType
prepActionImp opmode par_num (opcode, prev_pars, out_vals, in_state)
    | mode == 0 =
        if p_idx < 0
            then buildBoundsError p_idx
            else Right (opcode, prev_pars ++ [p_prog !! p_idx], out_vals, in_state { getProgram=p_prog })
    | mode == 1 = Right (opcode, prev_pars ++ [i_prog !! i_idx], out_vals, in_state { getProgram=i_prog })
    | mode == 2 =
        if r_idx < 0
            then buildBoundsError r_idx
            else Right (opcode, prev_pars ++ [r_prog !! r_idx], out_vals, in_state { getProgram=r_prog })
    | otherwise = throwError (UnsupportedInputMode mode)
    where mode = getParMode opmode par_num
          program = getProgram in_state
          i_idx = getIdx in_state + par_num
          i_prog = growMem i_idx program
          p_idx = i_prog !! i_idx
          p_prog = growMem p_idx i_prog
          r_idx = getRelIdx in_state + p_idx
          r_prog = growMem r_idx p_prog

-- list of resolved input values
-- A list of output mode, location value pair empty if no output.
-- The intcode state to use.
prepAction :: IntcodeState -> ComputeMonad PrepActionType
prepAction in_state =
    case qnum_pars of
        Nothing -> throwError (UnsupportedInstruction opcode)
        Just (num_in, has_out) ->
            if has_out
                then foldl (prepActionFold) (Right (opcode, [], [(getParMode opmode (num_in+1), program !! (idx+num_in+1))], in_state)) [1 .. num_in]
                else foldl (prepActionFold) (Right (opcode, [], [], in_state)) [1 .. num_in]
    where program = getProgram in_state
          idx = getIdx in_state
          opmode = program !! idx
          opcode = getOpcode opmode
          qnum_pars = Map.lookup opcode numPars
          prepActionFold :: ComputeMonad PrepActionType -> Int -> ComputeMonad PrepActionType
          prepActionFold acc idx = acc >>= (prepActionImp opmode idx)

-- An intcode function taking 2 arguments giving one output
type Inst21 = Int -> Int -> Int

-- Add instruction implementation
addInst :: Inst21
addInst a b = a+b

-- Mult instruction implementation
multInst :: Inst21
multInst a b = a*b

-- less than instruction implementation
lessInst :: Inst21
lessInst a b = if a < b then 1 else 0

-- equals instruction implementation
equalsInst :: Inst21
equalsInst a b = if a == b then 1 else 0

-- Handle input cmd
doInput :: PrepActionType -> ComputeMonad IntcodeState
doInput (_, _, outinfo, in_state)
    | getInput in_state == [] = throwError NoInputAvailable
    | otherwise = writeResult mode idx val in_state {getIdx=cur_idx+2, getInput=new_inputs}
    where (mode, idx) = head outinfo
          inputs = getInput in_state
          cur_idx = getIdx in_state
          val = head inputs
          new_inputs = drop 1 inputs

-- Handle output cmd
doOutput :: PrepActionType -> ComputeMonad IntcodeState
doOutput (_, in_vals, _, in_state) =
    Right in_state {getIdx = cur_idx+2, getOutput=outputs ++ [val]}
    where val = head in_vals
          outputs = getOutput in_state
          cur_idx = getIdx in_state

-- jump instructions
jumpCond :: (Int -> Bool) -> PrepActionType -> ComputeMonad IntcodeState
jumpCond cond (_, vals, outinfo, in_state)
    | cond a = Right $ in_state { getIdx=b }
    | otherwise = Right $ in_state { getIdx=idx+3 }
    where a = head vals
          b = head $ drop 1 vals
          idx = getIdx in_state

-- Adjust rel idx instruction
adjustRelIdx :: PrepActionType -> ComputeMonad IntcodeState
adjustRelIdx (_, vals, _, in_state) = Right $ in_state { getIdx=idx+2, getRelIdx=rel_idx+val }
    where idx = getIdx in_state
          rel_idx = getRelIdx in_state
          val = head vals

writeResult :: IntcodeMode -> IntcodePtr -> Int -> IntcodeState -> ComputeMonad IntcodeState
writeResult mode idx val state
    | mode == 0 && idx < 0 = throwError (MemoryUnderrun idx)
    | mode == 0 = Right $ state { getProgram=(U.replaceNth idx val p_program) }
    | mode == 2 = Right $ state { getProgram=(U.replaceNth r_idx val r_program) }
    | otherwise = throwError (UnsupportedOutputMode mode)
    where program = getProgram state
          p_program = growMem idx program        
          r_idx = (getRelIdx state) + idx
          r_program = growMem r_idx program
          cur_idx = getIdx state
          opmode = program !! cur_idx

-- Advance state function

advanceStateFunc21 :: Inst21 -> PrepActionType -> ComputeMonad IntcodeState
advanceStateFunc21 func (_, vals, outinfo, in_state) =
    writeResult mode idx (func a b) in_state {getIdx=cur_idx+4}
    where (mode, idx) = head outinfo
          a = head vals
          b = head $ drop 1 vals
          cur_idx = getIdx in_state

advanceStateHelper :: PrepActionType -> ComputeMonad IntcodeState
advanceStateHelper inv@(opcode, in_vals, outinfo, in_state)
    | opcode == 1 = advanceStateFunc21 addInst inv -- Addition
    | opcode == 2 = advanceStateFunc21 multInst inv -- Multiplication
    | opcode == 3 = doInput inv -- Input Handling
    | opcode == 4 = doOutput inv -- Output Handling
    | opcode == 5 = jumpCond (/=0) inv
    | opcode == 6 = jumpCond (==0) inv
    | opcode == 7 = advanceStateFunc21 lessInst inv -- less than
    | opcode == 8 = advanceStateFunc21 equalsInst inv -- equal to
    | opcode == 9 = adjustRelIdx inv
    | opcode == 99 = Right in_state
    | otherwise = throwError (UnsupportedInstruction opcode)
    where program = getProgram in_state
          cur_idx = getIdx in_state
          opmode = program !! cur_idx

advanceState :: IntcodeState -> ComputeMonad IntcodeState
advanceState state =
    case act_prep of
        Left x -> Left x
        Right y -> advanceStateHelper y
    where act_prep = prepAction state

-- Handle Nothing crashes and halt conditions
advanceCondition :: ComputeMonad IntcodeState -> Bool
advanceCondition (Left _) = False
advanceCondition (Right state) = if opcode == 99 then False else True
    where input = getInput state
          idx = getIdx state
          program = getProgram state
          opmode = program !! idx
          opcode = getOpcode opmode

advanceConditionWaitInput :: ComputeMonad IntcodeState -> Bool
advanceConditionWaitInput (Left _) = False
advanceConditionWaitInput (Right state) = if (opcode == 99) || ((opcode == 3) && (length input == 0)) then False else True
    where input = getInput state
          idx = getIdx state
          program = getProgram state
          opmode = program !! idx
          opcode = getOpcode opmode

isError :: ComputeMonad a -> Bool
isError input =
    case input of
        Left _ -> True
        Right _ -> False

-- Run the program and get output if it terminates properly
runProgram :: IntcodeInputs -> IntcodeProgram -> ComputeMonad IntcodeState
runProgram inputs state = head $ dropWhile advanceCondition $ iterate (>>=advanceState) (Right (initState state inputs))

runProgramTillNeedInput :: IntcodeState -> ComputeMonad IntcodeState
runProgramTillNeedInput init_state = head $ dropWhile advanceConditionWaitInput $ iterate (>>=advanceState) (Right init_state)

runProgramDebug :: Int -> IntcodeInputs -> IntcodeProgram -> [ComputeMonad IntcodeState]
runProgramDebug num_steps inputs state = take num_steps $ iterate (>>=advanceState) (Right (initState state inputs))
