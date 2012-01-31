module Language.PCF.Semantic where

import Language.PCF.Grammar

typeCheck = undefined

f x = x + 1

Lambda :: a0 -> a1) (Var "x" :: a0) (Add (Var "x" :: a2) (Nat 1 :: Nat))

-- remap bound vars

Lambda :: a0 -> a1) (Var "x" :: a0) (Add (Var "x" :: a0) (Nat 1 :: Nat))

-- 
