module Language.PCF.Semantic where

import Data.Map as M
import Language.PCF.Grammar

data TypeEq = TypeEq Type Type
              deriving (Show, Eq)

type TypeSystem = [TypeEq]

{- | Take a variable type and a specialization and substitute throughout the type system.
 -}
specialize :: Type -- ^ Var type
           -> Type -- ^ Specialized type.
           -> TypeSystem -- ^ Input type system
           -> TypeSystem -- ^ Output type system
specialize (VarT var) spec system = map specializeRule system
    where
        specializeRule (TypeEq t1 t2) = TypeEq (specializeType t1) (specializeType t2)
        specializeType t@(VarT a) | a == var  = spec
                                  | otherwise = t
        specializeType (ProdT a b) = ProdT (specializeType a) (specializeType b)
        specializeType (FuncT a b) = FuncT (specializeType a) (specializeType b)
        specializeType t           = t
specialize _ _ system = system


