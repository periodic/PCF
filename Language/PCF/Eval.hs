module Language.PCF.Eval ( lazy
                         , eager
                         , showEager
                         , showLazy
                         , showSteps
                         ) where

import Language.PCF.Grammar

isNormalForm :: Expr -> Bool
isNormalForm (BoolTrue)     = True
isNormalForm (BoolFalse)    = True
isNormalForm (Nat _)        = True
isNormalForm (Pair l r)     = isNormalForm l && isNormalForm r
isNormalForm _              = False


isReducible (Eq a b)             = redOrNorm [a,b]
isReducible (IfThenElse p _ _)   = redOrNorm [p]
isReducible (Add a b)            = redOrNorm [a,b]
isReducible (Pair a b)           = redOrNorm [a,b]
isReducible (Ap (Lambda _ _) _)  = True
isReducible (Ap f arg)           = isReducible f || isReducible arg
isReducible (Fix _)              = True
isReducible (Proj i e)           = case e of
                                    (Pair _ _) -> True
                                    _          -> isReducible e
isReducible _                    = False

redOrNorm es = any isReducible es || all isNormalForm es

{- | Lazy evaluation.
 -}
lazy (Eq a b) | isReducible a = Eq (lazy a) b
              | isReducible b = Eq a (lazy b)
              | otherwise     = case (a, b) of
                                    (Nat i1, Nat i2) -> if (i1 == i2) then BoolTrue else BoolFalse
                                    _                -> error "Comparison of non-numeric types."
lazy (IfThenElse BoolTrue t f) = t
lazy (IfThenElse BoolFalse t f) = f
lazy (IfThenElse pred t f) | isReducible pred   = IfThenElse (lazy pred) t f
                           | otherwise          = error "Non-reducible, non-boolean predicate."
lazy (Add a b) | isReducible a = Add (lazy a) b
               | isReducible b = Add a (lazy b)
               | otherwise     = case (a, b) of
                                    (Nat i1, Nat i2) -> Nat (i1 + i2)
                                    _                -> error "Addition of non-numeric types."
lazy (Pair a b) | isReducible a = Pair (lazy a) b
                | isReducible b = Pair a (lazy b)
                | otherwise     = Pair a b
lazy (Proj i e) | isReducible e = Proj i (lazy e)
                | otherwise     = case e of
                    Pair l r -> if i == 1 then l else r
                    _          -> error "Projection of non-pair type."
lazy (Ap (Lambda v body) arg) = substitute v arg body
lazy (Ap e               arg) = Ap (lazy e) arg
lazy (Fix f) = Ap f (Fix f)
lazy e = e

{- | Eager evaluation
 -}
eager (Eq a b) | isReducible a = Eq (eager a) b
               | isReducible b = Eq a (eager b)
               | otherwise     = case (a, b) of
                                    (Nat i1, Nat i2) -> if (i1 == i2) then BoolTrue else BoolFalse
                                    _                -> error "Comparison of non-numeric types."
eager (IfThenElse BoolTrue t f) = t
eager (IfThenElse BoolFalse t f) = f
eager (IfThenElse pred t f) | isReducible pred   = IfThenElse (eager pred) t f
                            | otherwise          = error "Non-reducible, non-boolean predicate."
eager (Add a b) | isReducible a = Add (eager a) b
                | isReducible b = Add a (eager b)
                | otherwise     = case (a, b) of
                                    (Nat i1, Nat i2) -> Nat (i1 + i2)
                                    _                -> error "Addition of non-numeric types."
eager (Pair a b) | isReducible a = Pair (eager a) b
                 | isReducible b = Pair a (eager b)
                 | otherwise     = Pair a b
eager (Proj i e) | isReducible e = Proj i (eager e)
                 | otherwise     = case e of
                                        Pair l r -> if i == 1 then l else r
                                        _        -> error "Projection of non-pair type."
eager (Ap l@(Lambda v body) arg) 
    | isReducible arg = Ap l (eager arg)
    | otherwise       = substitute v arg body
eager e@(Ap l arg) 
    | isReducible arg = Ap l (eager arg)
    | isReducible l   = Ap (eager l) arg
    | otherwise       = e
eager (Fix f) = Ap f (Fix f)
eager e = e

substitute :: Ident -> Expr -> Expr -> Expr
substitute v e = sub
    where
        sub (Eq a b)             = Eq (sub a) (sub b)
        sub (IfThenElse p t f)   = IfThenElse (sub p) (sub t) (sub f)
        sub (Add  a b)           = Add (sub a) (sub b)
        sub (Pair a b)           = Pair (sub a) (sub b)
        sub (Ap   f a)           = Ap   (sub f) (sub a)
        sub (Fix  f)             = Fix  (sub f)
        sub orig@(Var v1) | v == v1   = e
                          | otherwise = orig
        sub orig@(Lambda v1 b) | v == v1   = orig
                               | otherwise = Lambda v1 (sub b)
        sub e1 = e1 -- Fallthrough

showSteps :: (Expr->Expr) -> Expr -> [Expr]
showSteps eval e = showEvals e 100
    where
        showEvals e n = 
            if n <= 0
            then []
            else let e' = eval e
                 in if e == e'
                    then [e]
                    else e : showEvals e' (n - 1)

showLazy = showSteps lazy
showEager = showSteps eager
