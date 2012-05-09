module Language.PCF.Text.Pretty where

import Language.PCF.Grammar
import Text.Printf

prettyPrint :: ExprWCtx -> String
prettyPrint (ExprWCtx root ctx) = ppId root ctx

ppCtx :: Expr -> Context -> String
ppCtx (Var name)            _   = show name
ppCtx BoolTrue              _   = "true"
ppCtx BoolFalse             _   = "false"
ppCtx (Nat i)               _   = show i
ppCtx (Eq a b)              ctx = printf "(Eq? %s %s)"  (ppId a ctx) (ppId b ctx)
ppCtx (IfThenElse p a b)    ctx = printf "(if %s then %s else %s)"   (ppId p ctx) (ppId a ctx) (ppId b ctx)
ppCtx (Add a b)             ctx = printf "(%s + %s)"    (ppId a ctx) (ppId b ctx)
ppCtx (Pair a b)            ctx = printf "<%s, %s>"     (ppId a ctx) (ppId b ctx)
ppCtx (Proj i e)            ctx = printf "Proj_%i(%s)"  i            (ppId e ctx)
ppCtx (Lambda v body)       ctx = printf "(\\%s. %s)"   (show v)     (ppId body ctx)
ppCtx (Ap f arg)            ctx = printf "(%s %s)"      (ppId f ctx) (ppId arg ctx)
ppCtx (Fix f)               ctx = printf "(fix %s)"     (ppId f ctx)
ppCtx _                     _   = "(??)"


ppId :: ExprId -> Context -> String
ppId id ctx = case getExpr id ctx of
                Just (ExprData e _ _)  -> ppCtx e ctx
                Nothing -> "(??)"
