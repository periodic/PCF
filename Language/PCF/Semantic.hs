module Language.PCF.Semantic where

import Language.PCF.Grammar

import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Applicative

import Data.Map as M

data TypeEqn = TypeEqn Type Type
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
                            typeFor t typ
                            typeFor f typ
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
                            vType <- getTypeId var -- Make a type for this variable binding.
                            eType <- getTypeId e
                            addTypeEqn typ (FuncT vType eType)
                            withContext var vType $ gatherTypeEquations e -- Recurse with the new scope.
                        Ap a b -> do
                            f   <- getTypeId a
                            arg <- getTypeId b
                            typeFor a (FuncT arg typ)
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

type VarBindings = Map TypeId Type

type TypeSys = StateT TypeEqns (StateT VarBindings (WriterT [String] Identity))

type TypeData = Map TypeId Type

addError :: String -> TypeSys ()
addError = lift . lift . tell . (: [])

getType :: TypeId -> TypeSys Type
getType tid = lift $ do
    bindings <- get
    undefined

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

unifyOne :: Type -> Type -> TypeData
-- Nat
unifyOne (NatT) (NatT) = M.empty
-- Bool
unifyOne (BoolT) (BoolT) = M.empty
-- Vars and vars
unifyOne (VarT vid1) t@(VarT vid2) | vid1 == vid2 = M.empty
                                   | otherwise    = M.singleton vid1 t
-- Vars with other types.
unifyOne (VarT tid) t =
    if occurs tid t
    then error "not unifiable: circularity"
    else M.singleton tid t
unifyOne t (VarT tid) =
    if occurs tid t
    then error "not unifiable: circularity"
    else M.singleton tid t
-- Recursive types.
unifyOne (ProdT a1 b1) (ProdT a2 b2) = M.union (unifyOne a1 a2) (unifyOne b1 b2)
unifyOne (FuncT a1 b1) (FuncT a2 b2) = M.union (unifyOne a1 a2) (unifyOne b1 b2)
-- Anything else produces an error!
unifyOne _ _ = error "Not unifiable: different types"

unifyAll :: TypeEqns -> TypeData
unifyAll [] = M.empty
unifyAll ((TypeEqn t1 t2):eqs) = M.union sHead sTail'
    where
        sHead = unifyOne (applySub sTail t1) (applySub sTail t2)
        sTail = unifyAll eqs
        sTail' = applySubToSub sHead sTail


getTypeData :: ExprWCtx -> Either String TypeData
getTypeData expr = unifyAll <$> getTypeEquations expr
