{-# LANGUAGE ExistentialQuantification #-}
module Language.PCF.Grammar where

import Data.String
import Text.Printf

-- For Arbitrary instances.
import Control.Applicative
import Test.QuickCheck

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
instance IsString Ident where
    fromString = Ident

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
          | Undefined
          deriving (Show, Eq)

------------------------
-- # Arbitrary Instances
------------------------

instance Arbitrary Type where
    arbitrary = VarT . abs <$> arbitrary

instance Arbitrary Expr where
    arbitrary = sized sizedExpr

instance Arbitrary Ident where
    arbitrary = Ident <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

{- | Generate an expression, sized so it won't be infinite!
 -}
sizedExpr :: Int -> Gen Expr
sizedExpr n | n == 0    = oneof [ var, true, false, nat ]
            | otherwise =  oneof [ var, true, false, nat, eq, ite, add, pair, proj, lambda, ap, fix ]
    where
        var     = Var <$> arbitrary
        true    = return BoolTrue
        false   = return BoolFalse
        nat     = Nat . abs <$> arbitrary
        subExpr = sizedExpr (n `div` 2)
        eq      = Eq <$> simpleExpr <*> simpleExpr
        ite     = IfThenElse <$> subExpr <*> subExpr <*> subExpr
        add     = Add <$> subExpr <*> subExpr
        pair    = Pair <$> subExpr <*> subExpr
        proj    = Proj <$> elements [1..2] <*> subExpr
        lambda  = Lambda <$> arbitrary <*> subExpr
        ap      = Ap <$> simpleExpr <*> subExpr
        fix     = Fix <$> subExpr
        simpleExpr = oneof [ var, true, false, nat, eq, ite, pair, proj, lambda, fix ]

