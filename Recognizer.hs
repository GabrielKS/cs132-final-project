{-# LANGUAGE LambdaCase #-}
module Recognizer(recognize) where
import Language
import Lexer
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

-- Toggle debug printing
-- dtrace = trace  -- On
dtrace _ fun = fun  -- Off

data Result = Accept | Reject String deriving Show  -- All happy families are alike...
data Progress = Nopop | Pop Int Nonterm | End Result deriving Show  -- The various messages states need to pass up the chain
type StateFn = Progress -> [Token] -> (Progress, [Token])  -- States are of this type
data Nonterm = T | E | E' deriving (Eq, Ord, Show)  -- Nonterminals for the grammar

ts = getTokens "a+(b)"  -- Test string

recognize :: [Token] -> Result  -- The main function of import (no pun intended). Call it with some tokens and it tells you whether you have a string in the language.
recognize tokens = case state0 Nopop tokens of
    (End res, remnants) -> dtrace (show remnants) res
    (_, remnants) -> Reject $ "Exited parser with remaining tokens " ++ show remnants 

-- Some debug helper functions
printVars nToPop nt restTokens statename = dtrace ("\tnToPop=" ++ show nToPop ++ ", nt=" ++ show nt ++ ", state=" ++ show statename)
printStateStart statename tokens = dtrace ("state=" ++ show statename ++ ", tokens=" ++ show tokens)

-- All shift states are of this form
shiftConstructor :: String -> (Token -> Maybe StateFn) -> (Nonterm -> Maybe StateFn) -> StateFn
shiftConstructor _ _ _ (End res) tokens = (End res, tokens)
shiftConstructor statename _ _ _ [] = (End (Reject ("Empty input to shift state " ++ statename)), [])
shiftConstructor statename actionmap gotomap _ tokens@(h:t) = printStateStart statename tokens $ 
    let
        (prog, restTokens) = case actionmap h of
            Nothing -> (End (Reject ("No match for terminal " ++ show h ++ " in state " ++ statename)), tokens)
            Just f -> f Nopop t
    in
    handleGoto statename gotomap (prog, restTokens)

-- Helper function to handle the recursion involved in goto-ing
handleGoto :: String -> (Nonterm -> Maybe StateFn) -> (Progress, [Token]) -> (Progress, [Token])
handleGoto _ _ (End res, tokens) = (End res, tokens)
handleGoto _ _ (Nopop, tokens) = error $ "Malformed parser; should not be encountering a Nopop here. Tokens = " ++ show tokens
handleGoto statename gotomap (Pop nToPop nt, restTokens) = printVars nToPop nt restTokens statename $
            if nToPop > 0 then
                (Pop (nToPop-1) nt, restTokens)
            else
                case gotomap nt of
                    Nothing -> (End (Reject ("No match for non-terminal " ++ show nt ++ " in state " ++ statename)), restTokens)
                    Just f -> handleGoto statename gotomap (f Nopop restTokens)

-- All reduce states are of this form
reduceConstructor :: String -> Nonterm -> Int -> StateFn
reduceConstructor _ _ _ (End res) tokens = (End res, tokens)
reduceConstructor statename nt nToPop _ tokens = printStateStart statename tokens (Pop (nToPop-1) nt, tokens)

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
        TEnd  -> Just (\_ tokens -> (End Accept, tokens))  -- Function that tells shiftConstructor to accept
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
