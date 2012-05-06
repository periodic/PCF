{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PCF.Eval  where

import Control.Applicative
--import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import Language.PCF.Grammar


{- # Helper functions
 - -}

-- | Test if something is in normal form.
isNormalForm :: Expr -> Bool
isNormalForm (BoolTrue)     = True
isNormalForm (BoolFalse)    = True
isNormalForm (Nat _)        = True
isNormalForm (Pair l r)     = isNormalForm l && isNormalForm r
isNormalForm _              = False

-- | Test if something is reducible.
isReducible :: Expr -> Bool
isReducible (Eq a b)             = redOrNorm [a,b]
isReducible (IfThenElse p _ _)   = redOrNorm [p]
isReducible (Add a b)            = redOrNorm [a,b]
isReducible (Pair a b)           = redOrNorm [a,b]
isReducible (Ap (Lambda _ _) _)  = True
isReducible (Ap f arg)           = isReducible f || isReducible arg
isReducible (Fix _)              = True
isReducible (Proj _ e)           = case e of
                                    (Pair _ _) -> True
                                    _          -> isReducible e
isReducible _                    = False

-- | Test if a list of things is reducible or all in normal form.  Useful for implementing isReducible.
redOrNorm :: [Expr] -> Bool
redOrNorm es = any isReducible es || all isNormalForm es

{- # The Eval environment
 -}

data EvalConfig = EC { maxIters  :: Int               -- ^ Number of times this expression can be reduced.
                     , strategy  :: Expr -> Eval Expr -- ^ Strategy to use.
                     , context   :: Expr -> Expr
                     }

type EvalSteps = [Expr]

newtype Eval a = E {
    runEval :: WriterT EvalSteps (Reader EvalConfig) a
    } deriving (Monad, Functor, MonadReader EvalConfig, MonadWriter EvalSteps)

getStrategy :: Eval (Expr -> Eval Expr)
getStrategy = strategy <$> ask
getIterMax :: Eval Int
getIterMax = maxIters <$> ask
getContext :: Eval (Expr -> Expr)
getContext = context <$> ask

addContext :: (Expr -> Expr) -> Eval Expr -> Eval Expr
addContext c' = local modContext
    where
        modContext (EC i s c) = EC i s (c . c')

emit :: Expr -> Eval ()
emit e = do
    ctx <- getContext
    tell $ [ctx e]

eval :: (Expr -> Expr) -> Expr -> Eval Expr
eval ctx expr = addContext ctx $ getIterMax >>= loop expr 0
    where
        loop e n m =
            if n >= m
            then return Undefined
            else do
                strat <- getStrategy
                e' <- strat e
                if e' == e
                    then return e
                    else emit e' >> loop e' (n + 1) m

evaluate :: Expr -> EvalSteps
evaluate e = snd . flip runReader (EC 10 lazy id) . runWriterT . runEval $ (emit e >> eval id e)

{- | Lazy evaluation.
 -}
lazy :: Expr -> Eval Expr
lazy (Eq a b) = do
    a' <- eval (\e -> Eq e  b)  a
    b' <- eval (\e -> Eq a' e)  b
    case (a', b') of
        (Nat i1, Nat i2) -> if (i1 == i2) 
                            then return BoolTrue 
                            else return BoolFalse
        _                -> error "Comparison of non-numeric types."

lazy (IfThenElse p t f) = do
    pred' <- eval (\e -> IfThenElse e t f) p
    case pred' of
        BoolTrue    -> return t
        BoolFalse   -> return f
        _           -> error "Non-reducible, non-boolean predicate."

lazy (Add a b) = do
    a' <- eval (\e -> Add e  b) a
    b' <- eval (\e -> Add a' e) b
    case (a', b') of
        (Nat i1, Nat i2) -> return $ Nat (i1 + i2)
        _                -> error "Addition of non-numeric types."
lazy (Pair a b) = do
    a' <- eval (\e -> Pair e  b) a
    b' <- eval (\e -> Pair a' e) b
    return $ Pair a' b'
lazy (Proj i e) = do
    e' <- eval (\e_ -> Proj i e_) e
    case e' of
        Pair l r -> if i == 1 
                    then return l 
                    else return r
        _        -> error "Projection of non-pair type."
lazy (Ap e arg) = do 
    e' <- eval (\e_ -> Ap e_ arg) e
    case e' of
        (Lambda v body) -> return $ substitute v arg body
        _               -> error "Function application on non-lambda type."

lazy (Fix f) = return $ Ap f (Fix f)
lazy e = return e

{- | Eager evaluation
 -}
{-
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
-}

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
showSteps strat expr = showEvals expr 100
    where
        showEvals :: Expr -> Integer -> [Expr]
        showEvals e n = 
            if n <= 0
            then []
            else let e' = strat e
                 in if e == e'
                    then [e]
                    else e : showEvals e' (n - 1)

-- showLazy = showSteps lazy
-- showEager = showSteps eager
