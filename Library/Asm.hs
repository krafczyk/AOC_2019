module Library.Asm where

import qualified Library.Utility as U

-- The intcode state definition
type IntcodeState = (Int, [Int])

-- An intcode function taking 3 arguments
type Inst3 = Int -> Int -> Int -> [Int] -> [Int]

-- Utility function to apply a function with 3 arguments
applyInst3 :: Inst3 -> Int -> Int -> Int -> [Int] -> Maybe [Int]
applyInst3 func a b c state
    | a >= state_length = Nothing
    | b >= state_length = Nothing
    | c >= state_length = Nothing
    | otherwise = Just (func a b c state)
    where state_length = length state

-- Add instruction implementation
addInst :: Inst3
addInst a b c state = U.replaceNth c ((state !! a)+(state !! b)) state

-- Mult instruction implementation
multInst :: Inst3
multInst a b c state = U.replaceNth c ((state !! a)*(state !! b)) state

-- Advance state helper for 3 argument instructions
advanceStateFunc3 :: Inst3 -> IntcodeState -> Maybe [Int]
advanceStateFunc3 func (idx, state) = applyInst3 func (state !! (idx+1)) (state !! (idx+2)) (state !! (idx+3)) state

-- Advance state function
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

-- Handle Nothing crashes and halt conditions
advanceCondition :: Maybe IntcodeState -> Bool
advanceCondition Nothing = False
advanceCondition (Just (idx, state)) = if (state !! idx) == 99 then False else True
