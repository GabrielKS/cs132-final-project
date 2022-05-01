module Recognizer(recognize) where
import Language
import Lexer
import Debug.Trace
ts = getTokens "a+(b)"

type StateFn = [Token] -> (Nonterm, [Token])
data Nonterm = T | E | E' deriving (Show, Eq)

recognize :: [Token] -> Bool
recognize tokens = let (nt, rest) = recognize_s0 tokens in (nt == E' && null rest)

recurseGotomap :: (Nonterm -> StateFn) -> Nonterm -> [Token] -> (Nonterm, [Token])
recurseGotomap gotomap initialNonterm initialTokens =
    let (newNonterm, newTokens) = gotomap initialNonterm initialTokens in
        recurseGotomap gotomap newNonterm newTokens

chain :: (Token -> StateFn) -> (Nonterm -> StateFn) -> [Token] -> (Nonterm, [Token])
chain _ _ [] = error "Empty input to chain"
chain actionmap gotomap tokens@(h:t) = let (recognized, rest) = actionmap h t in recurseGotomap gotomap recognized rest

recognize_s0 :: StateFn
recognize_s0 [] = error "Empty input to recognize_s0"
recognize_s0 tokens@(h:t) = trace "s0" $ chain actionmap gotomap tokens
    where
        actionmap h = case h of
            TIdent _ -> recognize_s4
            TLParen  -> recognize_s3
            _ -> error "Error in shift s0"
        gotomap nt = case nt of
            E -> recognize_s1
            T -> recognize_s2
            _ -> error "Error in goto s0"

recognize_s1 :: StateFn
recognize_s1 [] = error "Empty input to recognize_s1"
recognize_s1 tokens@(h:t) = trace "s1" $ 
    case h of
        TPlus -> recognize_s5 t
        TEnd -> (E', [])
        _ -> error "Error in recognize_s1"

recognize_s2 :: StateFn
recognize_s2 tokens = trace "s2" $ (E, tokens)

recognize_s3 :: StateFn
recognize_s3 [] = error "Empty input to recognize_s3"
recognize_s3 tokens@(h:t) = trace "s3" $ chain shift goto t
    where
        shift h = case h of
            TIdent _ -> recognize_s4
            TLParen  -> recognize_s3
            _ -> error "Error in shift s3"
        goto nt = case nt of
            E -> recognize_s6
            T -> recognize_s2
            _ -> error "Error in goto s3"

recognize_s4 :: StateFn
recognize_s4 tokens = trace "s4" $ (T, tokens)

recognize_s5 :: StateFn
recognize_s5 [] = error "Empty input to recognize_s5"
recognize_s5 tokens@(h:t) = trace "s5" $ chain shift goto t
    where
        shift h = case h of
            TIdent _ -> recognize_s4
            TLParen  -> recognize_s3
            _ -> error "Error in shift s5"
        goto nt = case nt of
            T -> recognize_s8
            _ -> error "Error in goto s5"

recognize_s6 :: StateFn
recognize_s6 [] = error "Empty input to recognize_s6"
recognize_s6 tokens@(h:t) = trace "s6" $ 
    case h of
        TPlus -> recognize_s5 t
        TRParen -> recognize_s7 t
        _ -> error "Error in recognize_s6"

recognize_s7 :: StateFn
recognize_s7 tokens = trace "s7" $ (T, tokens)

recognize_s8 :: StateFn
recognize_s8 tokens = trace "s7" $ (E, tokens)
