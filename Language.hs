module Language where

-- Tokens (output of the lexer):
data Token = TIdent String | TLParen | TRParen | TPlus | TEnd deriving (Eq, Ord, Show)

-- Expressions (output AST of the parser):
data Expr = EAdd Expr Expr | EIdent String deriving Show

-- Nonterminals (not technically part of the language, but we put them here so both Recognizer and Parser can use them)
data Nonterm = T | E | E' deriving (Eq, Ord, Show)
