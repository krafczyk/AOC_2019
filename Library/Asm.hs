module Library.Asm where

import qualified Library.Utility as U
import Control.Monad.Except

-- Define the computation exception
data ComputeError = MemoryOverrun Int -- Attempted to access memory beyond the end
                  | MemoryUnderrun Int -- Attempted to access memory before the start of memory
                  | UnsupportedOutputMode Int -- Attempted to use an unsupported output mode
                  | BuildBoundsErrorUnexpected Int -- buildBounds Error used with value within bounds
                  | UnsupportedInstruction Int deriving (Eq)

instance Show ComputeError where
    show (MemoryOverrun idx) = "Attempted to access out memory beyond the end: " ++ (show idx)
    show (MemoryUnderrun idx) = "Attempted to access memory before the start!: " ++ (show idx)
    show (UnsupportedOutputMode mode) = "Tried to use non-position mode " ++ (show mode) ++ " with the output parameter"
    show (BuildBoundsErrorUnexpected idx) = "Used buildBoundsError with value within bounds! " ++ (show idx)
    show (UnsupportedInstruction opcode) = "Opcode " ++ (show opcode) ++ " is unsupported"

type ComputeMonad = Either ComputeError

-- The intcode state definition
type IntcodeState = (Int, [Int])

-- The intcode opmode type
type IntcodeOpmode = Int

-- The intcode opcode type
type IntcodeOpcode = Int

-- The intcode mode type
type IntcodeMode = Int

getOpcode :: IntcodeOpmode -> IntcodeOpcode
getOpcode opmode = opmode `mod` 100

getMode :: IntcodeOpmode -> IntcodeMode
getMode opmode = opmode `div` 100

getPar1Mode :: IntcodeMode -> Int
getPar1Mode mode = mode `mod` 10

getPar2Mode :: IntcodeMode -> Int
getPar2Mode mode = (mode `div` 10) `mod` 10

getPar3Mode :: IntcodeMode -> Int
getPar3Mode mode = (mode `div` 100) `mod` 10

-- An intcode function taking 2 arguments giving one output
type Inst21 = Int -> Int -> Int

-- Add instruction implementation
addInst :: Inst21
addInst a b = a+b

-- Mult instruction implementation
multInst :: Inst21
multInst a b = a*b

withinBounds :: Int -> Int -> Bool
withinBounds idx len = if (idx >= len)
                           then False
                           else if (idx < 0)
                               then False
                               else True

buildBoundsError :: Int -> Int -> ComputeMonad [Int]
buildBoundsError idx len = if (idx >= len)
                               then throwError (MemoryOverrun idx)
                               else if (idx < 0)
                                   then throwError (MemoryUnderrun idx)
                                   else throwError (BuildBoundsErrorUnexpected idx)

-- Utility function to apply a function with 2 arguments and one output
applyInst21 :: Inst21 -> IntcodeMode -> Int -> [Int] -> ComputeMonad [Int]
applyInst21 func mode idx state
    | par3mode /= 0 = throwError (UnsupportedOutputMode par3mode)
    | not $ withinBounds (idx+3) state_length = buildBoundsError (idx+3) state_length
    | (par1mode == 0) && (not $ withinBounds a state_length) = buildBoundsError a state_length
    | (par2mode == 0) && (not $ withinBounds b state_length) = buildBoundsError b state_length
    | not $ withinBounds c state_length = buildBoundsError c state_length
    | ((par1mode == 0) && (par2mode == 0)) = let v = func ai bi in
                                             Right $ U.replaceNth c v state
    | (par1mode == 0) = let v = func ai b in
                        Right $ U.replaceNth c v state
    | (par2mode == 0) = let v = func a bi in
                        Right $ U.replaceNth c v state
    | otherwise = let v = func a b in
                  Right $ U.replaceNth c v state
    where state_length = length state
          par1mode = getPar1Mode mode
          par2mode = getPar2Mode mode
          par3mode = getPar3Mode mode
          a = state !! (idx+1)
          b = state !! (idx+2)
          c = state !! (idx+3)
          ai = state !! a
          bi = state !! b
          ci = state !! c

-- Advance state helper for 2 argument 1 output instructions
advanceStateFunc21 :: Inst21 -> IntcodeMode -> IntcodeState -> ComputeMonad IntcodeState
advanceStateFunc21 func mode (idx, state) = let nextState = applyInst21 func mode idx state in
                                           case nextState of
                                               Left x -> Left x
                                               Right nState -> Right (idx+4, nState)

-- Advance state function
advanceState :: IntcodeState -> ComputeMonad IntcodeState
advanceState (idx, state)
    | opcode == 1 = advanceStateFunc21 addInst mode (idx,state)
    | opcode == 2 = advanceStateFunc21 multInst mode (idx,state)
    | opcode == 99 = Right (idx, state) -- This is the exit condition
    | otherwise = throwError (UnsupportedInstruction opcode)
    where opmode = state !! idx :: IntcodeOpmode
          opcode = getOpcode opmode
          mode = getMode opmode

-- Handle Nothing crashes and halt conditions
advanceCondition :: ComputeMonad IntcodeState -> Bool
advanceCondition (Left _) = False
advanceCondition (Right (idx, state)) = if (state !! idx) == 99 then False else True

-- Run the program and get output if it terminates properly
runProgram :: [Int] -> ComputeMonad Int
runProgram state = let final_val = (take 1 $ reverse $ U.takeWhileInclusive advanceCondition $ iterate (>>=advanceState) (Right (0, state))) !! 0 in
                   case final_val of
                       Left x -> Left x
                       Right (_,final_state) -> Right (final_state !! 0)
