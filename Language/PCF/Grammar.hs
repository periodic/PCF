{-# LANGUAGE ExistentialQuantification #-}
module Language.PCF.Grammar where

import Text.Printf

import Control.Applicative

data Type = VarT
          | NatT
          | BoolT
          | ProdT Type Type
          | FuncT Type Type

(<<?) :: Type -> Type -> Bool
_    <<? VarT = True -- Anything is a valid subtype of a variable
VarT <<? _    = False -- But any non-var is not a supertype of vars.
ProdT a1   b1   <<? ProdT a2 b2 = a1 <<? a2 && b1 <<? b2 -- products are covariant
FuncT arg1 ret1 <<? FuncT arg2 ret2 = arg2 <<? arg1 && ret1 <<? ret2 -- functions are contravariant in args, covariant in returns
_ <<? _ = False -- Anything else doesn't match

newtype Ident = Ident String
                deriving (Eq, Ord)

instance Show Ident where
    show (Ident str) = str

-- The grammar
--
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

instance Show Expr where
    show (Var name) = show name
    show BoolTrue  = "true"
    show BoolFalse = "false"
    show (Nat i)   = show i
    show (Eq a b)  = printf "Eq? %s %s" (show a) (show b)
    show (IfThenElse p a b) = printf "if %s then %s else %s" (show p) (show a) (show b)
    show (Add a b) = printf "%s + %s" (show a) (show b)
    show (Pair a b) = printf "<%s, %s>" (show a) (show b)
    show (Lambda v body) = printf "\\%s. %s" (show v) (show body)
    show (Let v b1 b2) = printf "let %s = %s in %s" (show v) (show b1) (show b2)
    show (LetRec f v b1 b2) = printf "letrec %s(%s) = %s in %s" (show f) (show v) (show b1) (show b2)

-- Constructors
{-
var :: String -> Expr
var = Var . Ident

boolTrue :: Expr
boolTrue = BoolTrue

boolFalse :: Expr
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
