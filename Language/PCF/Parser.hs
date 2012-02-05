{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Language.PCF.Parser ( runPCFParser
                           ) where

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
reservedWords = ["true", "false", "if", "then", "else", "fix"]

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
keyword s = (do
    spaces
    _ <- string s
    spaces) <?> "Keyword " ++ s

ident :: Stream s Identity Char => PCFParser s Ident
ident = do
    str <- some letter
    if str `elem` reservedWords
        then parserFail "Invalid identifier."
        else return $ Ident str

var, true, false, nat, eq, ifThenElse, pair, proj, lambda, fixp, parens, apLeft, infixExpr, simpleExpr, expr :: Stream s Identity Char => PCFParser s Expr

var = Var <$> try ident <?> "Var"
 
true  = BoolTrue <$ try (string "true") <?> "true"
false = BoolFalse <$ try (string "false") <?> "false"

nat = (Nat . read) <$> many1 digit <?> "nat"

eq = Eq
    <$ try (keyword "Eq?")
    <*> expr
    <* spaces
    <*> expr

ifThenElse = IfThenElse
    <$  try (keyword "if")
    <*> expr
    <*  keyword "then"
    <*> expr
    <*  keyword "else"
    <*> expr

pair = Pair
    <$ symbol '<'
    <*> expr
    <* symbol ','
    <*> expr
    <* termSymbol '>'

proj = Proj 
    <$ try (keyword "Proj_")
    <*> (read <$> many1 digit)
    <* spaces
    <*> expr

lambda = Lambda
    <$ symbol '\\'
    <*> ident
    <* symbol '.'
    <*> expr

parens =
    symbol '('
    *> expr
    <* termSymbol ')'

fixp = Fix
    <$ try (keyword "fix")
    <*> expr <?> "Fix"

simpleExpr = choice [fixp, lambda, pair, proj, ifThenElse, eq, nat, true, false, var, parens] <?> "Simple Expr"

apLeft = (do
    expr <- simpleExpr
    rest <- exprRest
    return $ foldl Ap expr rest) <?> "Function application"
    where
        exprRest = many1 $ try (choice [many1 space *> simpleExpr, parens])

infixExpr = chainl1 apLeft add
    where
        add = Add <$ try (symbol '+') <?> "Add"

expr = infixExpr <?> "Expression"

