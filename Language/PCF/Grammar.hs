{-# LANGUAGE ExistentialQuantification #-}
module Language.PCF.Grammar where

import Text.Printf

data Type = VarT Int
          | NatT
          | BoolT
          | ProdT Type Type
          | FuncT Type Type
          | InvalidT
          deriving (Eq)

instance Show Type where
    show (VarT i)       = printf "a%d" i
    show NatT           = "nat"
    show BoolT          = "bool"
    show (ProdT a b)    = printf "%s x %s" (show a) (show b)
    show (FuncT a b)    = printf "%s -> %s" (show a) (show b)
    show InvalidT       = "invalid"



(<<?) :: Type -> Type -> Bool
_               <<? (VarT _) = True -- Anything is a valid subtype of a variable
(VarT _)        <<? _        = False -- But any non-var is not a supertype of vars.
ProdT a1   b1   <<? ProdT a2 b2 = a1 <<? a2 && b1 <<? b2 -- products are covariant
FuncT arg1 ret1 <<? FuncT arg2 ret2 = arg2 <<? arg1 && ret1 <<? ret2 -- functions are contravariant in args, covariant in returns
_               <<? _ = False -- Anything else doesn't match

newtype Ident = Ident String
                deriving (Eq, Ord)

instance Show Ident where
    show (Ident str) = str

-- The grammar
--
data Expr = Var         Ident
          | BoolTrue    -- implicit type bool
          | BoolFalse   -- implicit type bool
          | Nat         Integer -- implicit type nat
          | Eq          Expr Expr -- implicit type bool
          | IfThenElse  Expr Expr Expr
          | Add         Expr Expr -- implicit type nat
          -- | Sub         Expr Expr -- implicit type nat
          | Pair        Expr Expr
          | Proj        Int Expr
          | Lambda      Ident Expr
          | Ap          Expr Expr
          | Fix         Expr
          deriving (Show, Eq)
