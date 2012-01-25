module Language.PCF.Parser where

import Text.Parsec

data Type = Nat
          | Bool
          | Func Type Type
          deriving (Show, Eq)

newtype Ident = Ident Str

-- The grammar
--
data Expr t = BoolTrue
            | BoolFalse
            | Nat Integer
            | Eq Expr Expr
            | IfThenElse (Expr Bool) (Expr t) (Expr t)
            | Add Expr Expr
            | Pair Expr Expr
            | Lambda Ident Expr
            | Let Ident Expr Expr
            | LetRec Ident Ident Expr Expr
            deriving (Show, Eq)

-- Constructors
boolTrue :: Expr Bool
boolTrue = BoolTrue

boolFalse :: Expr Bool
boolFalse = BoolTrue

nat :: Integer -> Expr Nat
nat i = Nat i

eq :: Expr a -> Expr a -> Expr Bool
eq t1 t2 = Eq t1 t2

-- The parser
var   = many1 letter >>= return . Ident
true  = string "true" >> return (BoolTrue Bool)
false = string "false" >> return (BoolFalse Bool)
eq    = do
    string "Eq?" 
    spaces
    t1 <- term
    spaces
    t2 <- term
    return $ Eq term term
ifthenelse = do
    string "if"
    spaces
    pred <- 


term = undefined
