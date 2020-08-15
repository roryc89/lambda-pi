{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaPi.Infer where 

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.List.Extra (allSame)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import LambdaPi.Term

-- Type Inferrence Erros

data TypeError 
    = TypeMismatch [Term]
    | NotAFunction Term
    | UnknownVariable Text
    | PatternDoesNotFitType Term Pattern
    | Errs TypeError TypeError
    deriving (Show, Eq)

collectErrors :: [TypeError] -> Maybe TypeError
collectErrors = foldr go Nothing
    where
        go :: TypeError -> Maybe TypeError -> Maybe TypeError
        go err (Just err2) = Just $ Errs err err2 
        go err Nothing = Just err

-- | Inference state
data InferState = 
    InferState 
        { count :: Int
        , typeEqs :: [TypeEquation]
        }

newInferState :: InferState
newInferState = InferState { count = 0, typeEqs = [] }

data Env = Env 
  { types :: Map.Map Text Term 
  , idxedTypes :: Map.Map Int Term
  }
  deriving (Eq, Show)

newEnv :: Env 
newEnv = Env Map.empty Map.empty 

-- | Inference monad
type Infer a = 
  ( ReaderT
      Env             -- Typing environment
      ( StateT         -- Inference state
          InferState
          (Except TypeError)
      )
      a
  )

runInferTerm ::  Term -> Either TypeError Term
runInferTerm term = 
    runExcept $ evalStateT (runReaderT go newEnv) newInferState
    where 
        go = do 
            inferred <- inferTerm term
            unifyTypeEqs
            pure inferred

type TypeEquation = (Term, Term)

-- To infer the type of a term 
-- we have easy cases like Int, String and Native functions. 
-- Then we have the more complex, lambda and apply.
-- For a lambda we have to find all the uses of the argument
-- If once used, it gets its type from its usage
-- If used in multiple places, it must be used the same way in all places
inferTerm :: Term -> Infer Term
inferTerm t_ = case t_ of 
    Int _ -> return typeInt

    String _ -> return typeString

    Ann term ann -> do 
        term `isOfType` ann
        return ann 
    
    App fn arg -> do
        fnT <- inferTerm fn
        case fnT of
            Arrow arrowArg body -> do
                addTypeEq arrowArg arg
                argT <- inferTerm arg
                return (replaceVarIdxWith arrowArg argT body)

            _ -> throwError $ NotAFunction fn

    Lam argName argAnn body -> do 
        argAnn <- maybe fresh return argAnn
        bodyT <- extendTypeEnv argName argAnn $ inferTerm body
        argInBody <- lookupTypeIdx argAnn 
        return (argInBody `Arrow` bodyT)

    Var name -> lookupType name 

    TypeConst t -> return Type 

    VarIdx i -> lookupIdxType i

    TypeVarDecl name annMay term -> 
        case annMay of 
            Nothing -> do 
                idxed <- fresh
                extendTypeEnv name idxed $ 
                    inferTerm term 
                
            Just ann ->
                extendTypeEnv name ann $ 
                    inferTerm term 

    Arrow t1 t2 -> return $ Arrow Type Type 

    Type -> return Type

    Case term branches -> undefined 
        -- do
        -- termT <- inferTerm term
        -- forM_ branches (\(p, res) -> do 
        --     fits <- patternFitsType termT p
        --     unless fits $
        --         throwError $ PatternDoesNotFitType termT p)

        -- return termT
    NatIntPlus1 t -> do 
        t `isOfType` typeInt
        return $ typeInt `Arrow` typeInt

    -- NatIntPlus termL termR -> intBinop termL termR 

    -- NatIntSub termL termR -> intBinop termL termR 
    
    -- NatIntMul termL termR -> intBinop termL termR 
    
    -- NatStringSlice start end str -> do
    --     start `isOfType` typeInt
    --     end `isOfType` typeInt
    --     str `isOfType` typeString 
    --     return $ typeInt `Arrow` typeInt `Arrow` typeString `Arrow` typeString 

    -- NatStringConcat str1 str2 -> do
    --     str1 `isOfType` typeString
    --     str2 `isOfType` typeString
    --     return $ typeString `Arrow` typeString `Arrow` typeString  

    where 
        notEqs t = return (t, [])

        intBinop termL termR = do
            termL `isOfType` typeInt
            termR `isOfType` typeInt
            return $ typeInt `Arrow` typeInt `Arrow` typeInt

patternFitsType :: Term -> Pattern -> Infer Bool 
patternFitsType termT p = case (termT, p) of 
    (Int _, PInt _) -> return True
    (String _, PString _) -> return True
    (_, PVar _) -> return True
    _ -> return False

isOfType :: Term -> Term -> Infer ()
isOfType term tipe = do 
    inferred <- inferTerm term 
    addTypeEq inferred tipe

addTypeEq :: Term -> Term -> Infer ()
addTypeEq t1 t2 = do 
    s <- get
    put s { typeEqs = (t1, t2) : typeEqs s }


-- | Get a new indexed term
fresh :: Infer Term
fresh = do
    s <- get
    put s {count = count s + 1}
    return $ VarIdx $ count s


lookupTypeIdx :: Term -> Infer Term
lookupTypeIdx (VarIdx i) = do 
    InferState{..} <- get 
    idxEqs <- getIdxedEquations typeEqs
    case Map.lookup i idxEqs of 
        Nothing -> pure $ VarIdx i
        Just [] -> pure $ VarIdx i
        Just [t] -> pure t
        Just terms@(term:tail) -> do 
            throwErrsIfNotEq [terms]
            pure term


lookupTypeIdx t = pure t
--     InferState{..} <- get  
--     let found = lookup i typeEqs 
--     case found of 
--         Nothing -> VarIdx i 
--         Just [t] -> t
--         Just many -> throwError undefined

unifyTypeEqs :: Infer ()
unifyTypeEqs = do 
    InferState{..} <- get 
    traceM "typeEqs"
    traceShowM typeEqs
    idxEqs <- getIdxedEquations typeEqs
    throwErrsIfNotEq (Map.elems idxEqs <> fmap getConcreteEq typeEqs)
    put $ InferState count []
    pure ()


getIdxedEquations :: [(Term, Term)] -> Infer (Map.Map Int [Term])
getIdxedEquations typeEqs = fst <$> foldM getEq (Map.empty, Map.fromList $ mapMaybe getIdxMap typeEqs) typeEqs

getConcreteEq :: (Term, Term) -> [Term]
getConcreteEq (t1, t2) = if isVarIdx t1 || isVarIdx t2 then [] else [t1, t2] 

getIdxMap :: (Term, Term) -> Maybe (Int, Int)
getIdxMap ts = case ts of 
    (VarIdx i1, VarIdx i2) -> pure $ if i2 > i1 then (i2, i1) else (i1, i2)
    _ -> Nothing

getEq :: 
    (Map.Map Int [Term], Map.Map Int Int) -> 
    (Term, Term) -> 
    Infer (Map.Map Int [Term], Map.Map Int Int) 
getEq ctx@(idxToTypes, idxs) ts = case ts of 
    (VarIdx i1, VarIdx i2) -> pure ctx
    (VarIdx i, t) -> insertTypeAtIdx i t
    (t, VarIdx i) -> insertTypeAtIdx i t
    (t1, t2) -> pure ctx -- if t1 == t2 then pure ctx else throwError $ TypeMismatch t1 t2
    where 
        insertTypeAtIdx i t = pure (Map.insertWith (<>) (getIdx i) [t] idxToTypes, idxs)
        getIdx i = maybe i getIdx $ Map.lookup i idxs


checkTypesEq :: [Term] -> Maybe TypeError
checkTypesEq ts = if allSame $ filter (not . isVarIdx) ts then Nothing else Just $ TypeMismatch ts 

isVarIdx t = case t of 
    VarIdx _ -> True 
    _ -> False 

throwErrsIfNotEq :: [[Term]] -> Infer ()
throwErrsIfNotEq terms = throwErrs $ mapMaybe checkTypesEq terms

throwErrs errs = case collectErrors errs of 
    Nothing -> pure ()
    Just errs -> throwError errs


-- | Extend type environment
extendTypeEnv :: Text -> Term -> Infer a -> Infer a
extendTypeEnv name term =
    local (\(Env types idxs) -> Env (Map.insert name term types) idxs)

extendIdxEnv :: Int -> Term -> Infer a -> Infer a
extendIdxEnv name term =
    local (\(Env types idxs) -> Env types (Map.insert name term idxs))

extendIdxEnvFromTerm :: Term -> Term -> Infer a -> Infer a
extendIdxEnvFromTerm term = case term of 
    VarIdx i -> extendIdxEnv i 
    _ -> \_ infer -> infer
   
    -- local (\(Env types idxs) -> Env types (Map.insert name term idxs))

lookupType :: Text -> Infer Term
lookupType name = do
    var <- lookupTypeMay name
    case var of 
        Nothing -> throwError $ UnknownVariable name 
        Just t -> return t

lookupTypeMay :: Text -> Infer (Maybe Term)
lookupTypeMay name = do
    (Env m idxs) <- ask 
    return $ Map.lookup name m

lookupIdxType :: Int -> Infer Term
lookupIdxType idx = do
    var <- lookupIdxMay idx
    case var of 
        Nothing -> throwError $ UnknownVariable $ T.pack $ show idx 
        Just t -> return t

lookupIdxMay :: Int -> Infer (Maybe Term)
lookupIdxMay idx = do
    (Env m idxs) <- ask 
    return $ Map.lookup idx idxs

getIdxSubstitution :: Term -> Infer Term
getIdxSubstitution t = case t of 
    VarIdx i -> fromMaybe t <$> lookupIdxMay i
    _ -> return t