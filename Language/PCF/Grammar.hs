{-# LANGUAGE ExistentialQuantification #-}
module Language.PCF.Grammar where

import Data.String
import Text.Printf
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Text.Parsec (SourcePos)

-- For Arbitrary instances.
import Control.Monad.State
import Control.Applicative
import Test.QuickCheck

newtype TypeId = TypeId Int deriving (Eq, Ord)
instance Show TypeId where
    show (TypeId i) = "a" ++ (show i)
nextTypeId :: TypeId -> TypeId
nextTypeId (TypeId i) = TypeId (i + 1)
startTypeId :: TypeId
startTypeId = TypeId 0

data Type = VarT TypeId
          | NatT
          | BoolT
          | ProdT Type Type
          | FuncT Type Type
          | InvalidT
          deriving (Eq)

instance Show Type where
    show (VarT i)       = show i
    show NatT           = "nat"
    show BoolT          = "bool"
    show (ProdT a b)    = printf "%s x %s" (show a) (show b)
    show (FuncT a b)    = printf "%s -> %s" (show a) (show b)
    show InvalidT       = "invalid"

(<<?) :: Type -> Type -> Bool
_               <<? (VarT _) = True -- Anything is a valid subtype of a variable
(VarT _)        <<? _        = False -- But any non-var is not a supertype of vars.
ProdT a1   b1   <<? ProdT a2 b2 = a1 <<? a2 && b1 <<? b2 -- products are covariant
FuncT arg1 ret1 <<? FuncT arg2 ret2 = arg2 <<? arg1 && ret1 <<? ret2 -- functions are contravariant in args, covariant in returns
_               <<? _ = False -- Anything else doesn't match

newtype Ident = Ident String
                deriving (Eq, Ord)

instance Show Ident where
    show (Ident str) = str
instance IsString Ident where
    fromString = Ident

newtype ExprId = ExprId Int
                deriving (Eq, Ord)

instance Show ExprId where
    show (ExprId i) = "Expr#" ++ (show i)

nextExprId :: ExprId -> ExprId
nextExprId (ExprId i) = ExprId (i + 1)

startExprId :: ExprId
startExprId = ExprId 0

-- The grammar
data Expr = Var         Ident
          | BoolTrue    -- implicit type bool
          | BoolFalse   -- implicit type bool
          | Nat         Integer -- implicit type nat
          | Eq          ExprId ExprId -- implicit type bool
          | IfThenElse  ExprId ExprId ExprId
          | Add         ExprId ExprId -- implicit type nat
          -- | Sub         ExprId ExprId -- implicit type nat
          | Pair        ExprId ExprId
          | Proj        Int ExprId
          | Lambda      Ident ExprId
          | Ap          ExprId ExprId
          | Fix         ExprId
          | Undefined
          deriving (Show, Eq)

data ExprData = ExprData 
    { edExpr :: Expr
    , edType :: Type
    , edPos  :: Maybe SourcePos
    } deriving (Show)

data Context = Context ExprId TypeId (IntMap ExprData) deriving (Show)
emptyContext :: Context
emptyContext = Context startExprId startTypeId M.empty

getExpr :: ExprId -> Context -> Maybe ExprData
getExpr (ExprId i) (Context _ _ mapping) = M.lookup i mapping

data ExprWCtx = ExprWCtx ExprId Context 

instance Show ExprWCtx where
    show (ExprWCtx root ctx) = (showIdInCtx root ctx)

showIdInCtx :: ExprId -> Context -> String
showIdInCtx eid ctx =
    let me = getExpr eid ctx
     in case me of 
        Just (ExprData e t _) -> showExprInCtx e ctx
        Nothing -> "??"

showExprInCtx :: Expr -> Context -> String
showExprInCtx e ctx =
    case e of
        Eq          id1 id2 -> printf "(Eq %s %s)" (showIdInCtx id1 ctx) (showIdInCtx id2 ctx)
        IfThenElse  id1 id2 id3 -> printf "(IfThenElse %s %s %s)"  (showIdInCtx id1 ctx) (showIdInCtx id2 ctx) (showIdInCtx id3 ctx)
        Add         id1 id2 -> printf "(Add %s %s)"  (showIdInCtx id1 ctx) (showIdInCtx id2 ctx)
        Pair        id1 id2 -> printf "(Pair %s %s)"  (showIdInCtx id1 ctx) (showIdInCtx id2 ctx)
        Proj        int id1 -> printf "(Proj %i %s)"  int (showIdInCtx id1 ctx)
        Lambda      ident id1 -> printf "(Lambda %s %s)"  (show ident) (showIdInCtx id1 ctx)
        Ap          id1 id2 -> printf "(Ap %s %s)"  (showIdInCtx id1 ctx) (showIdInCtx id2 ctx)
        Fix         id1 -> printf "(Fix %s)"  (showIdInCtx id1 ctx)
        other -> show other

------------------------
-- # BuildExpr Transformer
------------------------

type BuildExpr m = StateT Context m

addExpr :: Monad m => Expr -> BuildExpr m ExprId
addExpr e = do
    (Context eid@(ExprId i) tid ctx) <- get
    let eid'            = nextExprId eid
        tid'            = nextTypeId tid
        ctx'            = M.insert i (ExprData e (VarT tid) Nothing) ctx
    put (Context eid' tid ctx')
    return eid

runBuildExpr :: Monad m => BuildExpr m a -> m (a, Context)
runBuildExpr b = runStateT b emptyContext

makeExprWCtx :: Monad m => BuildExpr m ExprId -> m ExprWCtx
makeExprWCtx b = do
    (eid, ctx) <- runBuildExpr b
    return $ ExprWCtx eid ctx

------------------------
-- # Arbitrary Instances
------------------------

instance Arbitrary ExprWCtx where
    arbitrary = sized sizedExpr

instance Arbitrary Ident where
    arbitrary = Ident <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

{- | Generate an expression, sized so it won't be infinite!
 -}
sizedExpr :: Int -> Gen ExprWCtx
sizedExpr i = do
    (id, ctx) <- runBuildExpr (sizedExprInternal i)
    return $ ExprWCtx id ctx

sizedExprInternal :: Int -> BuildExpr Gen ExprId
sizedExprInternal n | n == 0    = leafExpr
                    | otherwise = allExpr
    where
        var, true, false, nat, eq, ite, add, pair, proj, lambda, ap, fix :: BuildExpr Gen Expr
        var     = Var <$> lift arbitrary
        true    = return BoolTrue
        false   = return BoolFalse
        nat     = Nat . abs <$> lift arbitrary
        eq      = Eq <$> simpleExpr <*> simpleExpr
        ite     = IfThenElse <$> subExpr <*> subExpr <*> subExpr
        add     = Add <$> subExpr <*> subExpr
        pair    = Pair <$> subExpr <*> subExpr
        proj    = Proj <$> lift (elements [1..2]) <*> subExpr
        lambda  = Lambda <$> lift arbitrary <*> subExpr
        ap      = Ap <$> simpleExpr <*> subExpr
        fix     = Fix <$> subExpr

        subExpr, leafExpr, simpleExpr, allExpr :: BuildExpr Gen ExprId
        subExpr = sizedExprInternal (n `div` 2)
        leafExpr   = exprOneOf [var, true, false, nat]
        simpleExpr = exprOneOf [var, true, false, nat, eq, ite, pair, proj, lambda, fix]
        allExpr    = exprOneOf [var, true, false, nat, eq, ite, add, pair, proj, lambda, ap, fix]
        exprOneOf :: [BuildExpr Gen Expr] -> BuildExpr Gen ExprId
        exprOneOf list = do
            ctx <- get
            (id, ctx') <- lift . oneof . map (\g -> runStateT (g >>= addExpr) ctx) $ list
            put ctx'
            return id



