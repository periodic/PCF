module Language.PCF.Test where

import Language.PCF.Grammar
import Language.PCF.Parser
import Language.PCF.Eval
import Language.PCF.Text.Pretty

import Control.Applicative
import Test.QuickCheck

instance Arbitrary Type where
    arbitrary = VarT . abs <$> arbitrary

instance Arbitrary Expr where
    arbitrary = sized sizedExpr

instance Arbitrary Ident where
    arbitrary = Ident <$> listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

{- | Common sub-exprs
 -}
var     = Var <$> arbitrary
true    = return BoolTrue
false   = return BoolFalse
nat     = Nat . abs <$> arbitrary
{- | Generate an expression, sized so it won't be infinite!
 -}
sizedExpr :: Int -> Gen Expr
sizedExpr 0 = oneof [ var, true, false, nat ]
sizedExpr n = oneof [ var, true, false, nat, eq, ite, add, pair, proj, lambda, ap, fix ]
    where
        subExpr = sizedExpr (n `div` 2)
        eq      = Eq <$> subExpr <*> subExpr
        ite     = IfThenElse <$> subExpr <*> subExpr <*> subExpr
        add     = Add <$> subExpr <*> subExpr
        pair    = Pair <$> subExpr <*> subExpr
        proj    = Proj <$> elements [1..2] <*> subExpr
        lambda  = Lambda <$> arbitrary <*> subExpr
        ap      = Ap <$> subExpr <*> subExpr
        fix     = Fix <$> subExpr

prop_PrintParse e = either (const False) (== e) . runPCFParser "TEST" . prettyPrint $ e
