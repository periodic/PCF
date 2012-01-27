{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Language.PCF.Parser where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Identity
import Data.Map as M
import Text.Parsec

import Language.PCF.Grammar

data St = St (Map Ident Type)

type PCFParser s = Parsec s St

runPCFParser p = runParser p (St M.empty)

addFreeVariable :: Ident -> Type -> PCFParser s ()
addFreeVariable i t = modifyState (insertVar) 
    where
        insertVar (St vars) = St $ insert i t vars

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
          | Let Ident Expr Expr
          | LetRec Ident Ident Expr Expr
-}

-- handy aliases
some :: Stream s Identity Char => PCFParser s o -> PCFParser s [o]
some = many1

ident :: Stream s Identity Char => PCFParser s Ident
ident = Ident <$> some letter

var, true, false, eq, ifThenElse, pair, add, lambda, letdef, letrec, term :: Stream s Identity Char => PCFParser s Expr

var   = Var <$> ident

true  = string "true" >> return BoolTrue
false = string "false" >> return BoolFalse

nat = do
    digits <- some digit
    return . Nat $ read digits

eq = do
    string "Eq?" 
    spaces
    t1 <- term
    spaces
    t2 <- term
    return $ Eq t1 t2

ifThenElse = do
    string "if"
    spaces
    pred <- term
    spaces
    string "then"
    spaces
    bTrue <- term
    spaces
    string "else"
    spaces
    bFalse <- term
    return $ IfThenElse pred bTrue bFalse

add = do
    t1 <- term
    spaces
    string "+"
    spaces
    t2 <- term
    return $ Add t1 t2

pair = do
    char '('
    spaces
    t1 <- term
    spaces
    char ','
    spaces
    t2 <- term
    spaces
    char ')'
    return $ Pair t1 t2

lambda = do
    char '\\'
    spaces
    v <- ident
    spaces
    char '.'
    spaces
    t <- term
    return $ Lambda v t

letdef = do
    string "let"
    spaces
    v <- ident
    spaces
    char '='
    spaces
    t <- term
    spaces
    string "in"
    spaces
    b <- term
    return $ Let v t b

letrec = do
    string "letrec"
    spaces
    f <- ident
    spaces
    char '('
    spaces
    arg <- ident
    spaces
    char ')'
    spaces
    char '='
    spaces
    t <- term
    spaces
    string "in"
    spaces
    b <- term
    return $ LetRec f arg t b

-- TODO: Add needs left-factoring.
-- TODO: Simplify the long dos.
-- TODO: Add parens.
term = letrec <|> letdef <|> lambda <|> pair <|> ifThenElse <|> eq <|> nat <|> true <|> false <|> var
