{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module LambdaPi.Infer where 

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import LambdaPi.Term

-- Type Inferrence

data TypeError 
    = TypeMismatch Term Term
    | NotAFunction Term
    | UnknownVariable Text
    deriving (Show, Eq)

-- | Inference state
newtype InferState = 
    InferState 
        { count :: Int
        }

newInferState :: InferState
newInferState = InferState { count = 0 }

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
    runExcept $ evalStateT (runReaderT (inferTerm term) newEnv) newInferState

inferTerm :: Term -> Infer Term
inferTerm t_ = case t_ of 
    Int _ -> return typeInt

    String _ -> return typeString

    Ann term ann -> do 
        inferred <- inferTerm term
        checkTypeMatch ann inferred
        return inferred 
    
    App fn arg -> do
        fnT <- inferTerm fn
        case fnT of
            Arrow arrowArg body -> do
                checkTypeMatchNoVars arrowArg arg
                argT <- inferTerm arg
                return $ replaceVarIdxWith arrowArg argT body

            _ -> throwError $ NotAFunction fn

    Lam argName argAnn body -> do 
        argT <- maybe fresh return argAnn
        bodyT <- extendTypeEnv argName argT $ inferTerm body
        return $ argT `Arrow` bodyT 

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

    NatIntPlus termL termR -> intBinop termL termR 

    NatIntSub termL termR -> intBinop termL termR 
    
    NatIntMul termL termR -> intBinop termL termR 
    
    NatStringSlice start end str -> do
        start `isOfType` typeInt
        end `isOfType` typeInt
        str `isOfType` typeString 
        return $ typeInt `Arrow` typeInt `Arrow` typeString `Arrow` typeString 

    NatStringConcat str1 str2 -> do
        str1 `isOfType` typeString
        str2 `isOfType` typeString
        return $ typeString `Arrow` typeString `Arrow` typeString  

    where 
        intBinop termL termR = do
            termL `isOfType` typeInt
            termR `isOfType` typeInt
            return $ typeInt `Arrow` typeInt `Arrow` typeInt


isOfType :: Term -> Term -> Infer ()
isOfType term tipe = do 
    inferred <- inferTerm term 
    checkTypeMatch inferred tipe


checkTypeMatch :: Term -> Term -> Infer ()
checkTypeMatch expected actual = do
    expectedSub <- getIdxSubstitution expected
    actualSub <- getIdxSubstitution actual
    when (expectedSub /= actualSub) $ 
        throwError $ TypeMismatch expectedSub actualSub

checkTypeMatchNoVars :: Term -> Term -> Infer ()
checkTypeMatchNoVars expected actual = 
    when (notVarIdx expected && notVarIdx actual) $ 
        checkTypeMatch expected actual
    where 
        notVarIdx t = case t of 
            VarIdx _ -> False
            _ -> True

-- | Get a new indexed term
fresh :: Infer Term
fresh = do
    s <- get
    put s {count = count s + 1}
    return $ VarIdx $ count s

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