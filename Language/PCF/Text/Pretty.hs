module Language.PCF.Text.Pretty where

import Language.PCF.Grammar
import Text.Printf

prettyPrint :: Expr -> String
prettyPrint (Var name) = show name
prettyPrint BoolTrue  = "true"
prettyPrint BoolFalse = "false"
prettyPrint (Nat i)   = show i
prettyPrint (Eq a b)  = printf "(Eq? %s %s)" (prettyPrint a) (prettyPrint b)
prettyPrint (IfThenElse p a b) = printf "(if %s then %s else %s)" (prettyPrint p) (prettyPrint a) (prettyPrint b)
prettyPrint (Add a b) = printf "(%s + %s)" (prettyPrint a) (prettyPrint b)
-- prettyPrint (Sub a b) = printf "(%s - %s)" (prettyPrint a) (prettyPrint b)
prettyPrint (Pair a b) = printf "<%s, %s>" (prettyPrint a) (prettyPrint b)
prettyPrint (Proj i e) = printf "Proj_%i(%s)" i (prettyPrint e)
prettyPrint (Lambda v body) = printf "(\\%s. %s)" (show v) (prettyPrint body)
prettyPrint (Ap f arg) = printf "(%s %s)" (prettyPrint f) (prettyPrint arg)
prettyPrint (Fix f) = printf "(fix %s)" (prettyPrint f)

