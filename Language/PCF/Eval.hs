module Language.PCF.Eval where

import Language.PCF.Grammar

isNormalForm :: Expr -> Bool
isNormalForm (BoolTrue)     = True
isNormalForm (BoolFalse)    = True
isNormalForm (Nat _)        = True
isNormalForm (Pair t l r)   = isNormalForm l && isNormalForm r
isNormalForm _              = False

isReducible e = not $ isNormalForm e

eval (Eq a b) | isReducible a = Eq (eval a) b
              | isReducible b = Eq a (eval b)
              | otherwise     = case (a, b) of
                                    (Nat i1, Nat i2) -> if (i1 == i2) then BoolTrue else BoolFalse
                                    _                -> error "Comparison of non-numeric types."
eval (IfThenElse _ BoolTrue t f) = t
eval (IfThenElse _ BoolFalse t f) = f
eval (IfThenElse typ pred t f) | isReducible pred   = IfThenElse typ (eval pred) t f
                               | otherwise          = error "Non-reducible, non-boolean predicate."
eval (Add a b) | isReducible a = Add (eval a) b
               | isReducible b = Add a (eval b)
               | otherwise     = case (a, b) of
                                    (Nat i1, Nat i2) -> Nat (i1 + i2)
                                    _                -> error "Addition of non-numeric types."
eval (Pair t a b) | isReducible a = Pair t (eval a) b
                  | isReducible b = Pair t a (eval b)
                  | otherwise     = Pair t a b
eval (Proj t i e) | isReducible e = Proj t i (eval e)
                  | otherwise     = case e of
                    Pair _ l r -> if i == 1 then l else r
                    _          -> error "Projection of non-pair type."
eval (Ap _ (Lambda _ v body) arg) = substitute v arg body
eval (Ap t e                 arg) = Ap t (eval e) arg
eval (Fix t f) = Ap t f (Fix t f)
eval e = e


substitute :: Ident -> Expr -> Expr -> Expr
substitute v e = sub
    where
        sub (Eq a b)               = Eq (sub a) (sub b)
        sub (IfThenElse typ p t f) = IfThenElse typ (sub p) (sub t) (sub f)
        sub (Add    a b)           = Add (sub a) (sub b)
        sub (Pair t a b)           = Pair t (sub a) (sub b)
        sub (Ap   t f a)           = Ap   t (sub f) (sub a)
        sub (Fix  t f)             = Fix  t (sub f)
        sub orig@(Var _ v1) | v == v1   = e
                            | otherwise = orig
        sub orig@(Lambda t v1 b) | v == v1   = orig
                                 | otherwise = Lambda t v1 (sub b)
        sub e1 = e1 -- Fallthrough

showSteps :: Expr -> [Expr]
showSteps e = showEvals e 100
    where
        showEvals e n = 
            if n <= 0
            then []
            else let e' = eval e
                 in if e == e'
                    then [e]
                    else e : showEvals e' (n - 1)
