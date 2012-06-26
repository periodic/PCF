{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Language.PCF.Parser ( runPCFParser
                           , run
                           ) where

import Control.Applicative hiding (many)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec

import Language.PCF.Grammar

type PCFInternal = BuildExpr Identity
type PCFParser s = ParsecT s () PCFInternal

runPCFParser :: forall s. Stream s PCFInternal Char => SourceName -> s -> Either ParseError ExprWCtx
runPCFParser src stream = case runIdentity . runBuildExpr $ runParserT expr () src stream of
    (Left a, _) -> Left a
    (Right id, ctx) -> Right $ ExprWCtx id ctx 

-- Test function
run :: forall s. Stream s PCFInternal Char => PCFParser s ExprId -> s -> Either ParseError ExprWCtx
run e stream = case runIdentity . runBuildExpr $ runParserT e () "TEST" stream of
    (Left a, _) -> Left a
    (Right id, ctx) -> Right $ ExprWCtx id ctx 

storeSubExpr :: forall s. Stream s PCFInternal Char => PCFParser s Expr -> PCFParser s ExprId
storeSubExpr exprP = do
    e <- exprP
    id <- lift . addExpr $ e
    return id

symbol :: Stream s PCFInternal Char => Char -> PCFParser s ()
symbol c = do
    spaces
    _ <- char c
    spaces

-- Symbols that terminate expressions
termSymbol :: Stream s PCFInternal Char => Char -> PCFParser s ()
termSymbol c = do
    spaces
    _ <- char c
    return ()

keyword :: Stream s PCFInternal Char => String -> PCFParser s ()
keyword s = (do
    spaces
    _ <- string s
    spaces) <?> "Keyword " ++ s

ident :: Stream s PCFInternal Char => PCFParser s Ident
ident = do
    str <- some letter
    if str `elem` reservedWords
        then parserFail "Invalid identifier."
        else return $ Ident str
    where
        reservedWords = ["true", "false", "if", "then", "else", "fix"]

var, true, false, nat, eq, ifThenElse, pair, proj, lambda, fixp :: Stream s PCFInternal Char => PCFParser s Expr

var = Var <$> try ident <?> "Var"

true  = BoolTrue <$ try (string "true") <?> "true"
false = BoolFalse <$ try (string "false") <?> "false"

nat = (Nat . read) <$> many1 digit <?> "nat"

eq = Eq
    <$ try (keyword "Eq?")
    <*> simpleExpr -- No application in EQ params unless they are parenthesized.
    <* spaces
    <*> simpleExpr -- No application in EQ params unless they are parenthesized.

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
    <*> simpleExpr

lambda = Lambda
    <$ symbol '\\'
    <*> (storeSubExpr $ var)
    <* symbol '.'
    <*> expr

fixp = Fix
    <$ try (keyword "fix")
    <*> expr <?> "Fix"

apLeft, parens, simpleExpr, infixExpr, expr :: Stream s PCFInternal Char => PCFParser s ExprId

parens =
    symbol '('
    *> expr
    <* termSymbol ')' <?> "Parenthized expression"


simpleExpr = choice [storeSubExpr . choice $ [fixp, lambda, pair, proj, ifThenElse, eq, nat, true, false, var], parens]

apLeft = do
    first <- simpleExpr
    rest <- exprRest
    foldM (\a b -> storeSubExpr . return $ Ap a b) first rest <?> "Function application"
    where
        exprRest = many $ try (choice [many1 space *> simpleExpr, parens])

infixExpr = do
    first <- apLeft
    rest <- exprRest
    foldM (\a b -> storeSubExpr . return $ Add a b) first rest <?> "Addition"
    where
        --chainlm p f = p *> f <* p
        --add = (\a b -> storeSubExpr . return $ Add a b) <$ try (symbol '+') <?> "Add")
        exprRest = many $ try (symbol '+' *> apLeft)

expr = infixExpr <?> "Expression"

