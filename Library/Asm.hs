module Library.Asm where

import qualified Library.Utility as U

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
addInst a b c state = U.replaceNth c ((state !! a)+(state !! b)) state

multInst :: Inst3
multInst a b c state = U.replaceNth c ((state !! a)*(state !! b)) state

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

