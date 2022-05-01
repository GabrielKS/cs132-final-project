{-# LANGUAGE LambdaCase #-}
module Recognizer(recognize) where
import Language
import Lexer
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

type StateFn = Int -> [Token] -> (Int, Nonterm, [Token])
data Nonterm = T | E | E' deriving (Eq, Ord, Show)

ts = getTokens "a+(b)"

recognize = state0 0

printVars nToPop nt restTokens statename = trace ("\tnToPop=" ++ show nToPop ++ ", nt=" ++ show nt ++ ", state=" ++ show statename)
printStateStart statename tokens = trace ("state=" ++ show statename ++ ", tokens=" ++ show tokens)

shiftConstructor :: String -> (Token -> Maybe StateFn) -> (Nonterm -> Maybe StateFn) -> StateFn
shiftConstructor statename _ _ _ [] = error $ "Empty input to shift state " ++ statename
shiftConstructor statename actionmap gotomap pToPop tokens@(h:t) = printStateStart statename tokens $ 
    let
        (nToPop, initNt, restTokens) = case actionmap h of
            Nothing -> error $ "No match for terminal " ++ show h ++ " in state " ++ statename
            Just f -> f 0 t
    in
    handleGoto statename gotomap (nToPop, initNt, restTokens)

handleGoto :: String -> (Nonterm -> Maybe StateFn) -> (Int, Nonterm, [Token]) -> (Int, Nonterm, [Token])
handleGoto statename gotomap (nToPop, nt, restTokens) = printVars nToPop nt restTokens statename $
            if nToPop > 0 then
                (nToPop-1, nt, restTokens)
            else
                case gotomap nt of
                    Nothing -> error $ "No match for non-terminal " ++ show nt ++ " in state " ++ statename
                    Just f -> handleGoto statename gotomap (f 0 restTokens)

reduceConstructor :: String -> Nonterm -> Int -> StateFn
reduceConstructor statename nt nToPop _ tokens = printStateStart statename tokens (nToPop-1, nt, tokens)

state0 :: StateFn
state0 = shiftConstructor "0"
    (\case
        TIdent _ -> Just state4
        TLParen  -> Just state3
        _ -> Nothing)
    (\elem -> Map.lookup elem (Map.fromList [(E, state1), (T, state2)]))
    
state1 :: StateFn
state1 = shiftConstructor "1"
    (\case
        TPlus -> Just state5
        TEnd  -> Just state0  -- TODO accept
        _ -> Nothing)
    (const Nothing)

state2 :: StateFn
state2 = reduceConstructor "2" E 1

state3 :: StateFn
state3 = shiftConstructor "3"
    (\case
        TIdent _ -> Just state4
        TLParen  -> Just state3
        _ -> Nothing)
    (\elem -> Map.lookup elem (Map.fromList [(E, state6), (T, state2)]))

state4 :: StateFn
state4 = reduceConstructor "4" T 1

state5 :: StateFn
state5 = shiftConstructor "5"
    (\case
        TIdent _ -> Just state4
        TLParen  -> Just state3
        _ -> Nothing)
    (\elem -> Map.lookup elem (Map.fromList [(T, state8)]))

state6 :: StateFn
state6 = shiftConstructor "6"
    (\case
        TPlus   -> Just state5
        TRParen -> Just state7
        _ -> Nothing)
    (const Nothing)

state7 :: StateFn
state7 = reduceConstructor "7" T 3

state8 :: StateFn
state8 = reduceConstructor "8" E 3
