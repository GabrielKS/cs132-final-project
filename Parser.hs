{-# LANGUAGE LambdaCase #-}
module Parser(parse, recognize, parseStr, recognizeStr) where
import Language
import Lexer
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

-- Toggle debug printing
-- dtrace = trace  -- On
dtrace _ fun = fun  -- Off

data ParseResult = Success Expr | Failure String deriving Show  -- Either succeed and provide an AST or fail and tell us why
data RecognizeResult = Accept | Reject String deriving Show  -- ...each unhappy family is unhappy in its own way
data Progress = Nopop | Pop Int Nonterm | End RecognizeResult deriving Show  -- The various messages states need to pass up the chain
type StateTup = (Progress, [Token], [Token], [Production])  -- All the information we need to keep track of the state: message, tokens remaining, tokens consumed, productions generated
type StateFn = StateTup -> StateTup  -- States are of this type
data Element = ET Token | ENT Nonterm deriving Show  -- An element of the grammar is either a terminal or a non-terminal
type Production = (Nonterm, [Element])  -- Production rules are of this type

-- Some test strings:
ts1 = getTokens "a+(b)"
ts2 = getTokens "a+(b+c)"
ts3 = getTokens "(a+b)+c"
ts4 = getTokens "a+b+c+d"

-- Shortcuts to lex and parse/recognize at the same time
parseStr :: String -> ParseResult
parseStr = parse . getTokens
recognizeStr :: String -> RecognizeResult
recognizeStr = recognize . getTokens

-- Given tokens, either parse them into an AST or provide an error message
parse :: [Token] -> ParseResult
parse tokens = case state0 (Nopop, tokens, [], []) of
    (End Accept, _, _, productions) -> Success (productionsToAST productions)
    (End (Reject s), _, _, _) -> Failure s
    (_, remnants, _, theParse) -> Failure $ "Exited parser with remaining tokens " ++ show remnants ++ " and parse " ++ show theParse

-- Turn a rightmost derivation into an AST (in linear time)
productionsToAST :: [Production] -> Expr
productionsToAST prods = let (exp, rest) = productionsToASTHelper prods in
    if null rest then exp else error "Malformed parser; should not be exiting productionsToASTHelper with remaining productions"

-- This could be generalized without too much difficulty
productionsToASTHelper :: [Production] -> (Expr, [Production])
productionsToASTHelper [] = error "Malformed parser; should not be passing empty list to productionsToASTHelper"
productionsToASTHelper ((T, [ET (TIdent s)]):rest) = (EIdent s, rest)
productionsToASTHelper ((E,[ENT E,ET TPlus,ENT T]):rest) =
    let (rhs, rest2) = productionsToASTHelper rest in
        let (lhs, rest3) = productionsToASTHelper rest2 in
            (EAdd lhs rhs, rest3)
productionsToASTHelper (h:t) = productionsToASTHelper t

-- Given tokens, either verify that they are a valid string in the language or provide an error message
recognize :: [Token] -> RecognizeResult
recognize tokens = case state0 (Nopop, tokens, [], []) of
    (End res, _, _, theParse) -> dtrace (show theParse) res
    (_, remnants, _, theParse) -> Reject $ "Exited parser with remaining tokens " ++ show remnants ++ " and parse " ++ show theParse

-- Some debug helper functions
printVars nToPop nt restTokens statename = dtrace ("\tnToPop=" ++ show nToPop ++ ", nt=" ++ show nt ++ ", state=" ++ show statename)
printStateStart statename tokens = dtrace ("state=" ++ show statename ++ ", tokens=" ++ show tokens)

-- All shift states are of this form
shiftConstructor :: String -> (Token -> Maybe StateFn) -> (Nonterm -> Maybe StateFn) -> StateFn
shiftConstructor _ _ _ input@(End res, _, _, _) = input
shiftConstructor statename _ _ (_, [], done, theParse) = (End (Reject ("Empty input to shift state " ++ statename)), [], done, theParse)
shiftConstructor statename actionmap gotomap (_, tokens@(h:t), done, theParse) = printStateStart statename tokens $ 
    let
        res = case actionmap h of
            Nothing -> (End (Reject ("No match for terminal " ++ show h ++ " in state " ++ statename)), tokens, done, theParse)
            Just f -> f (Nopop, t, h:done, theParse)
    in
    handleGoto statename gotomap res

-- Helper function to handle the recursion involved in goto-ing
handleGoto :: String -> (Nonterm -> Maybe StateFn) -> StateFn
handleGoto _ _ input@(End res, _, _, _) = input
handleGoto _ _ (Nopop, tokens, _, _) = error $ "Malformed parser; should not be encountering a Nopop here. Tokens = " ++ show tokens
handleGoto statename gotomap (Pop nToPop nt, restTokens, done, theParse) = printVars nToPop nt restTokens statename $
            if nToPop > 0 then
                (Pop (nToPop-1) nt, restTokens, done, theParse)
            else
                case gotomap nt of
                    Nothing -> (End (Reject ("No match for non-terminal " ++ show nt ++ " in state " ++ statename)), restTokens, done, theParse)
                    Just f -> handleGoto statename gotomap (f (Nopop, restTokens, done, theParse))

-- All reduce states are of this form
reduceConstructor :: String -> ([Token] -> Production) -> StateFn
reduceConstructor _ _ (End res, tokens, done, theParse) = (End res, tokens, done, theParse)
reduceConstructor statename prodmap (_, tokens, done, theParse) =
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
        TEnd  -> Just (\(_, tokens, done, theParse) -> (End Accept, tokens, done, theParse))  -- Function that tells shiftConstructor to accept
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
