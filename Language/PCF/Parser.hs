{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Language.PCF.Parser where

import Control.Applicative
import Control.Monad.Identity
import Data.Map as M
import Text.Parsec

import Language.PCF.Grammar

data St = St { varTypes :: Map Ident Type
             , numVars  :: Int
             } deriving (Show)

type PCFParser s = Parsec s St

runPCFParser :: forall s. Stream s Identity Char => SourceName -> s -> Either ParseError Expr
runPCFParser = runParser expr (St M.empty 0)

addFreeVariable :: Ident -> PCFParser s ()
addFreeVariable i = do
    (St vars n) <- getState
    putState (St (insert i (VarT n) vars) (n + 1))

newTypeVar :: PCFParser s Type
newTypeVar = do
    (St vars n) <- getState
    putState (St vars (n + 1))
    return $ VarT n

-- The parser
{-
data Expr = Var Ident
          | BoolTrue
          | BoolFalse
          | Nat Integer
          | Eq Expr Expr
          | IfThenElse Expr Expr Expr
          | Add Expr Expr
          | Pair Expr Expr
          | Lambda Ident Expr
-}

-- handy aliases

symbol :: Stream s Identity Char => Char -> PCFParser s ()
symbol c = do
    spaces
    _ <- char c
    spaces

-- Symbols that terminate expressions
termSymbol :: Stream s Identity Char => Char -> PCFParser s ()
termSymbol c = do
    spaces
    _ <- char c
    return ()

keyword :: Stream s Identity Char => String -> PCFParser s ()
keyword s = do
    spaces
    _ <- string s
    spaces


ident :: Stream s Identity Char => PCFParser s Ident
ident = Ident <$> some letter

var, true, false, nat, eq, ifThenElse, pair, proj, lambda, fixp, parens, simpleExpr, binaryExpr, expr :: Stream s Identity Char => PCFParser s Expr

var = Var <$> newTypeVar
    <*> ident

true  = BoolTrue <$ try (string "true")
false = BoolFalse <$ try (string "false")

nat = (Nat . read) <$> many1 digit

eq = Eq
    <$ keyword "Eq?"
    <*> binaryExpr
    <* spaces
    <*> binaryExpr

ifThenElse = IfThenElse <$> newTypeVar
    <* try (keyword "if")
    <*> binaryExpr
    <* keyword "then"
    <*> binaryExpr
    <* keyword "else"
    <*> binaryExpr

pair = Pair <$> newTypeVar
    <* symbol '<'
    <*> expr
    <* symbol ','
    <*> expr
    <* termSymbol '>'

proj = Proj <$> newTypeVar
    <* try (keyword "Proj_")
    <*> (read <$> many1 digit)
    <* spaces
    <*> expr

lambda = Lambda <$> newTypeVar
    <* symbol '\\'
    <*> ident
    <* symbol '.'
    <*> expr

fixp = Fix <$> newTypeVar
    <* try (keyword "fix")
    <*> expr

binOps :: Stream s Identity Char => PCFParser s (Expr -> Expr -> Expr)
binOps = choice [add] --, sub]
    where
        add = Add <$ try (symbol '+')
        --sub = Sub <$ try (symbol '-')

apOp :: Stream s Identity Char => PCFParser s (Expr -> Expr -> Expr)
apOp = (Ap  <$> newTypeVar <* try (many1 space))

parens =
    symbol '('
    *> expr
    <* termSymbol ')'

simpleExpr = choice [fixp, lambda, pair, proj, ifThenElse, eq, nat, true, false, var, parens]
binaryExpr = chainl1 simpleExpr binOps

expr = chainl1 binaryExpr apOp

