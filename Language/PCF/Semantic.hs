module Language.PCF.Semantic where

import Language.PCF.Grammar

import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Applicative

import Data.Map as M

data TypeEqn = TypeEqn TypeId Type Type
              deriving (Show, Eq)
type TypeEqns = [TypeEqn]
type Scope = Map Ident TypeId
type TypeGatherer =  ReaderT Scope (ReaderT Context (WriterT TypeEqns (StateT TypeId (ErrorT String Identity))))

getTypeEquations :: ExprWCtx -> Either String TypeEqns
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

addTypeEqn :: TypeId -> Type -> TypeGatherer ()
addTypeEqn t1 t2 = lift . lift . tell $ [TypeEqn t1 (VarT t1) t2]

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

getTypeId :: ExprId -> TypeGatherer TypeId
getTypeId eid = do
    ctx <- getContext
    case getExpr eid ctx of
        Just (ExprData _ typ _) -> return typ
        Nothing                 -> typeError "Expression not found.  Malformed AST."

withContext :: ExprId -> Type -> TypeGatherer a -> TypeGatherer a
withContext vid (VarT tid) action = do
    ctx <- getContext
    case (getExpr vid ctx) of
        Just (ExprData (Var ident) _ _) -> withReaderT (M.insert ident tid) action
        Nothing                         -> typeError "Variable not found.  Malformed AST."

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
                            typeFor t (VarT typ)
                            typeFor f (VarT typ)
                            gatherTypeEquations p
                            gatherTypeEquations t
                            gatherTypeEquations f
                        Add a b -> do
                            addTypeEqn typ NatT
                            typeFor a NatT
                            typeFor b NatT
                            gatherTypeEquations a
                            gatherTypeEquations b
                        Pair a b -> do
                            aTyp <- getTypeId a
                            bTyp <- getTypeId b
                            addTypeEqn typ (ProdT (VarT aTyp) (VarT bTyp))
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
                            vType <- getTypeId var -- Make a type for this variable binding.
                            eType <- getTypeId e
                            addTypeEqn typ (FuncT (VarT vType) (VarT eType))
                            withContext var (VarT vType) $ gatherTypeEquations e -- Recurse with the new scope.
                        Ap a b -> do
                            f   <- getTypeId a
                            arg <- getTypeId b
                            typeFor a (FuncT (VarT arg) (VarT typ))
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

type TypeData = Map TypeId Type

occurs :: TypeId -> Type -> Bool
occurs tid (VarT vid) = tid == vid
occurs tid (ProdT t1 t2) = occurs tid t1 || occurs tid t2
occurs tid (FuncT t1 t2) = occurs tid t1 || occurs tid t2
occurs tid _             = False

substitute :: TypeId -> Type -> Type -> Type
substitute tid s t@(VarT vid) | vid == tid = s
                                | otherwise  = t
substitute tid s (ProdT t1 t2) = (ProdT $ substitute tid s t1) $ substitute tid s t2
substitute tid s (FuncT t1 t2) = (FuncT $ substitute tid s t1) $ substitute tid s t2
substitute _ _ t = t

applySub :: TypeData -> Type -> Type
applySub subst t = M.foldWithKey substitute t subst

applySubToSub :: TypeData -> TypeData -> TypeData
applySubToSub subs targ = M.map (applySub subs) targ

unifyOne :: TypeId -> Type -> Type -> TypeData
-- Invalid
unifyOne _ (InvalidT _) _ = M.empty
unifyOne _ _ (InvalidT _) = M.empty
-- Nat
unifyOne _ (NatT) (NatT) = M.empty
-- Bool
unifyOne _ (BoolT) (BoolT) = M.empty
-- Vars and vars
unifyOne _ (VarT vid1) t@(VarT vid2) | vid1 == vid2 = M.empty
                                   | otherwise    = M.singleton vid1 t
-- Vars with other types.
unifyOne _ (VarT tid) t =
    if occurs tid t
    then M.singleton tid (InvalidT "Circularity")
    else M.singleton tid t
unifyOne _ t (VarT tid) =
    if occurs tid t
    then M.singleton tid (InvalidT "Circularity")
    else M.singleton tid t
-- Recursive types.
unifyOne t0 (ProdT a1 b1) (ProdT a2 b2) = M.union (unifyOne t0 a1 a2) (unifyOne t0 b1 b2)
unifyOne t0 (FuncT a1 b1) (FuncT a2 b2) = M.union (unifyOne t0 a1 a2) (unifyOne t0 b1 b2)
-- Anything else produces an error!
unifyOne t0 a b = M.singleton t0 (InvalidT $ "Expected " ++ show a ++ " but got " ++ show b)

unifyAll :: TypeEqns -> TypeData
unifyAll [] = M.empty
unifyAll ((TypeEqn t0 t1 t2):eqs) = M.union sHead sTail'
    where
        sTail = unifyAll eqs
        sHead = unifyOne t0 (applySub sTail t1) (applySub sTail t2)
        sTail' = applySubToSub sHead sTail


getTypeData :: ExprWCtx -> Either String TypeData
getTypeData expr = unifyAll <$> getTypeEquations expr
