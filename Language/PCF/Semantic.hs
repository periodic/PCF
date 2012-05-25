module Language.PCF.Semantic where

import Language.PCF.Grammar

import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Map as M

data TypeEqn = TypeEqn Type Type
              deriving (Show, Eq)
type Scope = Map Ident TypeId
type TypeGatherer =  ReaderT Scope (ReaderT Context (WriterT [TypeEqn] (StateT TypeId (ErrorT String Identity))))

getTypeEquations :: ExprWCtx -> Either String [TypeEqn]
getTypeEquations (ExprWCtx root ctx) = fmap snd  -- extract the output of the writer
                                     . runIdentity -- Unwrap from identity
                                     . runErrorT  -- Map to an Either type.
                                     . flip evalStateT (startTypeId "s") -- Eval using a new type namespace.
                                     . runWriterT -- Run with the rule gathering.
                                     . flip runReaderT ctx -- Run with context
                                     . flip runReaderT (M.empty) -- Run with scope.
                                     $ gatherTypeEquations root

typeError :: String -> TypeGatherer a
typeError msg = throwError msg

addTypeEqn :: Type -> Type -> TypeGatherer ()
addTypeEqn t1 t2 = lift . lift . tell $ [TypeEqn t1 t2]

getVarType :: Ident -> TypeGatherer Type
getVarType ident = do
    mType <- do
                m <- ask
                return $ M.lookup ident m
    case mType of
        Nothing -> typeError "Variable is not in scope?"
        Just tid -> return $ VarT tid

getContext :: TypeGatherer Context
getContext = lift $ ask

typeFor :: ExprId -> Type -> TypeGatherer ()
typeFor eid typ = do
    t <- getTypeId eid
    addTypeEqn t typ

newType :: TypeGatherer Type
newType = lift . lift . lift $ do
    tid <- get
    let tid' = nextTypeId tid
    put tid'
    return $ VarT tid'

getTypeId :: ExprId -> TypeGatherer Type
getTypeId eid = do
    ctx <- getContext
    case getExpr eid ctx of
        Just (ExprData _ typ _) -> return typ
        Nothing                 -> typeError "Expression not found.  Malformed AST."

withContext :: Ident -> Type -> TypeGatherer a -> TypeGatherer a
withContext ident (VarT tid) = withReaderT (M.insert ident tid)

gatherTypeEquations :: ExprId -> TypeGatherer ()
gatherTypeEquations eid = do
            ctx <- getContext
            case getExpr eid ctx of
                Nothing -> typeError "Expression not found.  Malformed AST."
                Just (ExprData expr typ _) ->
                    case expr of
                        BoolTrue  -> addTypeEqn typ BoolT
                        BoolFalse -> addTypeEqn typ BoolT
                        Nat _     -> addTypeEqn typ NatT
                        Var var   -> do
                            vTyp <- getVarType var
                            addTypeEqn typ vTyp
                        Eq a b    -> do
                            addTypeEqn typ BoolT
                            typeFor a NatT
                            typeFor b NatT
                            gatherTypeEquations a
                            gatherTypeEquations b
                        IfThenElse p t f -> do
                            typeFor p BoolT
                            typeFor t typ
                            typeFor f typ
                            gatherTypeEquations p
                            gatherTypeEquations t
                            gatherTypeEquations f
                        Add a b -> do
                            addTypeEqn typ  NatT
                            typeFor a NatT
                            typeFor b NatT
                            gatherTypeEquations a
                            gatherTypeEquations b
                        Pair a b -> do
                            aTyp <- getTypeId a
                            bTyp <- getTypeId b
                            addTypeEqn typ (ProdT aTyp bTyp)
                            gatherTypeEquations a
                            gatherTypeEquations b
                        Proj i e -> do
                            v1 <- newType
                            v2 <- newType
                            typeFor e (ProdT v1 v2)
                            case i of
                                1 -> addTypeEqn typ v1
                                2 -> addTypeEqn typ v2
                                _ -> typeError "Projection with invalid index."
                            gatherTypeEquations e
                        Lambda var e -> do
                            vType <- newType
                            eType <- getTypeId e
                            addTypeEqn typ (FuncT vType eType)
                            withContext var vType $ gatherTypeEquations e
                        Ap a b -> do
                            t1 <- newType
                            t2 <- newType
                            typeFor a (FuncT t1 t2)
                            typeFor b t2
                            addTypeEqn typ t2
                            gatherTypeEquations a
                            gatherTypeEquations b
                        Fix e -> do
                            t <- newType
                            typeFor e (FuncT t t)
                            addTypeEqn typ t
                            gatherTypeEquations e
                        Undefined -> do
                            t <- newType
                            addTypeEqn typ t


type TypeSys = StateT [TypeEqn] (WriterT [String] Identity)

addError :: String -> TypeSys ()
addError = lift . tell

reduceRules :: [TypeEqn] -> ([TypeEqn], [String])

{-
meet :: Type -> Type -> Type
meet NatT NatT = NatT
meet BoolT BoolT = BoolT
meet (ProdT a1 b1) (ProdT a2 b2) = ProdT (meet a1 a2) (meet b1 b2)
meet (FuncT a1 b1) (FuncT a2 b2) | b1 == b2 = FuncT (meet a1 a2) b1
                                 | otherwise = InvalidT
meet _ _ = InvalidT

(<<?) :: Type -> Type -> Bool
_               <<? (VarT _) = True -- Anything is a valid subtype of a variable
(VarT _)        <<? _        = False -- But any non-var is not a supertype of vars.
ProdT a1   b1   <<? ProdT a2 b2 = a1 <<? a2 && b1 <<? b2 -- products are covariant
FuncT arg1 ret1 <<? FuncT arg2 ret2 = arg2 <<? arg1 && ret1 <<? ret2 -- functions are contravariant in args, covariant in returns
_               <<? _ = False -- Anything else doesn't match
-}
