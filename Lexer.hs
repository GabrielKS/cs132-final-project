module Lexer(getTokens) where
import Language

getTokens :: String -> [Token]
getTokens "" = [TEnd]
getTokens s@(h:t)
    | h == '(' = TLParen : getTokens t
    | h == ')' = TRParen : getTokens t
    | h == '+' = TPlus : getTokens t
    | isWhitespace h = getTokens t
    | otherwise = let (thisIdent, rest) = getIdent s in thisIdent : getTokens rest

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n']
isReserved :: Char -> Bool
isReserved c = c `elem` ['(', ')', '+']
isIdent :: Char -> Bool
isIdent c = not (isWhitespace c) && not (isReserved c)

getIdent input = let (soFar, rest) = getIdentHelper "" input in (TIdent (reverse soFar), rest)
getIdentHelper :: String -> String -> (String, String)
getIdentHelper soFar "" = (soFar, "")
getIdentHelper soFar s@(h:t) = if isIdent h then getIdentHelper (h : soFar) t else (soFar, s)
