{-# LANGUAGE LambdaCase #-}
module Parser(parse) where
import Language
import Lexer
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

-- Toggle debug printing
dtrace = trace  -- On
-- dtrace _ fun = fun  -- Off

data Result = Accept | Reject String deriving Show  -- All happy families are alike...
data Progress = Nopop | Pop Int Nonterm | End Result deriving Show  -- The various messages states need to pass up the chain
type StateFn = Progress -> [Token] -> [Token] -> [Production] -> (Progress, [Token], [Token], [Production])  -- States are of this type
data Element = ET Token | ENT Nonterm deriving Show
type Production = (Nonterm, [Element])

ts = getTokens "a+(b)"  -- Test string

parse :: [Token] -> Expr
parse = undefined

recognize :: [Token] -> Result  -- The main function of import (no pun intended). Call it with some tokens and it tells you whether you have a string in the language.
recognize tokens = case state0 Nopop tokens [] [] of
    (End res, remnants, done, theParse) -> dtrace (show theParse) res
    (_, remnants, done, theParse) -> Reject $ "Exited parser with remaining tokens " ++ show remnants ++ " and parse " ++ show theParse

-- Some debug helper functions
printVars nToPop nt restTokens statename = dtrace ("\tnToPop=" ++ show nToPop ++ ", nt=" ++ show nt ++ ", state=" ++ show statename)
printStateStart statename tokens = dtrace ("state=" ++ show statename ++ ", tokens=" ++ show tokens)

-- All shift states are of this form
shiftConstructor :: String -> (Token -> Maybe StateFn) -> (Nonterm -> Maybe StateFn) -> StateFn
shiftConstructor _ _ _ (End res) tokens done theParse = (End res, tokens, done, theParse)
shiftConstructor statename _ _ _ [] done theParse = (End (Reject ("Empty input to shift state " ++ statename)), [], done, theParse)
shiftConstructor statename actionmap gotomap _ tokens@(h:t) done theParse = printStateStart statename tokens $ 
    let
        (prog, restTokens, newDone, newParse) = case actionmap h of
            Nothing -> (End (Reject ("No match for terminal " ++ show h ++ " in state " ++ statename)), tokens, done, theParse)
            Just f -> f Nopop t (h:done) theParse
    in
    handleGoto statename gotomap (prog, restTokens, newDone, newParse)

-- Helper function to handle the recursion involved in goto-ing
handleGoto :: String -> (Nonterm -> Maybe StateFn) -> (Progress, [Token], [Token], [Production]) -> (Progress, [Token], [Token], [Production])
handleGoto _ _ (End res, tokens, done, theParse) = (End res, tokens, done, theParse)
handleGoto _ _ (Nopop, tokens, done, theParse) = error $ "Malformed parser; should not be encountering a Nopop here. Tokens = " ++ show tokens
handleGoto statename gotomap (Pop nToPop nt, restTokens, done, theParse) = printVars nToPop nt restTokens statename $
            if nToPop > 0 then
                (Pop (nToPop-1) nt, restTokens, done, theParse)
            else
                case gotomap nt of
                    Nothing -> (End (Reject ("No match for non-terminal " ++ show nt ++ " in state " ++ statename)), restTokens, done, theParse)
                    Just f -> handleGoto statename gotomap (f Nopop restTokens done theParse)

-- All reduce states are of this form
reduceConstructor :: String -> ([Token] -> Production) -> StateFn
reduceConstructor _ _ (End res) tokens done theParse = (End res, tokens, done, theParse)
reduceConstructor statename prodmap _ tokens done theParse =
    let prod@(src, dest) = prodmap done in
    printStateStart statename tokens (Pop (length dest - 1) src, tokens, done, prod : theParse)

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
        TEnd  -> Just (\_ tokens done theParse -> (End Accept, tokens, done, theParse))  -- Function that tells shiftConstructor to accept
        _ -> Nothing)
    (const Nothing)

state2 :: StateFn
state2 = reduceConstructor "2" (const (E, [ENT T]))

state3 :: StateFn
state3 = shiftConstructor "3"
    (\case
        TIdent _ -> Just state4
        TLParen  -> Just state3
        _ -> Nothing)
    (\elem -> Map.lookup elem (Map.fromList [(E, state6), (T, state2)]))

state4 :: StateFn
state4 = reduceConstructor "4" (\case
        (TIdent s) : _ -> (T, [ET (TIdent s)])
        _ -> error "Malformed parser; should be encountering a TIdent here")

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
state7 = reduceConstructor "7" (const (T, [ET TLParen, ENT E, ET TRParen]))

state8 :: StateFn
state8 = reduceConstructor "8" (const (E, [ENT E, ET TPlus, ENT T]))
