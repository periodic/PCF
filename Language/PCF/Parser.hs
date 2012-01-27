{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Language.PCF.Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.Map
import Text.Parsec

import Language.PCF.Grammar

data St = forall t. St (Map Ident Type)

type PCFParser s = Parsec s St

addFreeVariable :: Ident -> Type -> PCFParser s ()
addFreeVariable i t = modifyState (insertVar) 
    where
        insertVar (St vars) = St $ insert i t vars

{-
var :: String -> Expr a
var = Var . Ident

boolTrue :: Expr BoolT
boolTrue = BoolTrue

boolFalse :: Expr BoolT
boolFalse = BoolTrue

nat :: Integer -> Expr NatT
nat i = Nat i

eq :: Expr a -> Expr a -> Expr BoolT
eq t1 t2 = Eq t1 t2

add :: Expr NatT -> Expr NatT -> Expr NatT
add = Add

pair :: Expr t1 -> Expr t2 -> Expr (ProdT t1 t2)
pair = Pair

lambda :: Ident -> Expr t1 -> Expr (FuncT a t1)
lambda = Lambda

letDef :: Ident -> Expr t1 -> Expr t2 -> Expr t2
letDef = Let

letRec :: Ident -> Ident -> Expr t1 -> Expr t2 -> Expr t2
letRec = LetRec
-}

-- The parser
ident :: Stream s Identity Char => PCFParser s Ident
ident = Ident <$> many1 letter

var, true, false, eq, ifthenelse, term :: Stream s Identity Char => PCFParser s Expr

var   = Var <$> ident
true  = string "true" >> return BoolTrue
false = string "false" >> return BoolFalse

eq = do
    string "Eq?" 
    spaces
    t1 <- term
    spaces
    t2 <- term
    return $ Eq term term
ifthenelse = do
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


term = undefined
