{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaPi.Infer where 

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Text (Text)
import LambdaPi.Env
import LambdaPi.Term

-- Type Inferrence

data TypeError 
    = TypeMismatch Term Term
    | NotAFunction Term
    deriving (Show, Eq)

-- | Inference state
newtype InferState = InferState { count :: Int }


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

fresh :: Infer Term
fresh = do
    s <- get
    put s {count = count s + 1}
    return $ VarIdx $ count s

inferTerm :: [(Text, Term)] -> Term -> Infer Term
inferTerm ctx t_ = case t_ of 
    Int _ -> return typeInt

    String _ -> return typeString

    Ann term ann -> do 
        inferred <- inferTerm ctx term
        when (inferred /= ann)
            $ throwError $ TypeMismatch ann inferred 
        return inferred 
    
    App fn arg -> do 
        fnT <- inferTerm ctx fn
        case fnT of 
            Lam argName argAnn body -> do
                argT <- inferTerm ctx arg

                case argAnn of 
                    Nothing -> pure ()
                    Just argAnn_ -> 
                        when (argAnn_ /= argT)
                            $ throwError $ TypeMismatch argAnn_ argT

                inferTerm ((argName, arg) : ctx) body

            _ -> throwError $ NotAFunction fnT

    Lam argName argAnn body -> do 
        idxed <- fresh
        bodyT <- inferTerm ctx body
        return $ idxed `Arrow` bodyT 

    TypeConst t -> return Type 

    VarIdx i -> return Type

    TypeVarDecl name annMay term -> 
        case annMay of 
                Nothing -> do 
                    idxed <- fresh
                    inferTerm ((name, idxed) : ctx) term 
                    
                Just ann ->
                    inferTerm ((name, ann) : ctx) term 

    Arrow t1 t2 -> return $ Arrow Type Type 

    Type -> return Type 




