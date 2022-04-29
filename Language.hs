module Language where

-- Tokens (output of the lexer):
data Token = TIdent String | TLParen | TRParen | TPlus deriving Show

-- Expressions (output AST of the parser):
data Expr = EAdd Expr Expr | EIdent String deriving Show
