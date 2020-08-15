{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Andromeda.Infer where

import Andromeda.InferType
import Andromeda.Expr
import Andromeda.Error
import Andromeda.Ctx (extend, lookupValue, lookupType)
import Control.Monad.Except
import Control.Monad.Extra ((&&^))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

runInferType :: Expr -> Either TypeError Expr 
runInferType expr = 
    runExcept $ evalStateT (runReaderT go newEnv) newInferState
    where 
        go = inferType expr


inferType :: Expr -> Infer Expr
inferType expr = 
    case expr of 
        Var v -> lookupType v

        Universe k -> pure $ Universe $ k + 1

        Pi (x, t1, t2) -> do 
            k1 <- inferUniverse t1
            k2 <- inferUniverse t2
            pure $ Universe (max k1 k2)

        Lam (var, t, e) -> do
            inferUniverse t 
            te <- extend var t Nothing $ inferType e
            pure $ Pi (var, t, te)

        App e1 e2 -> do 
            (x, s, t) <- inferPi e1 
            te <- inferType e2
            checkEqual s te 
            substitute [(x, e2)] t


inferUniverse :: Expr -> Infer Int 
inferUniverse tipe = do 
    u <- normalize =<< inferType tipe 
    case u of 
        Universe k -> pure k 
        _ -> throwError $ TypeExpected u
    
inferPi :: Expr -> Infer Abstraction
inferPi e = do 
    u <- normalize =<< inferType e 
    case u of 
        Pi k -> pure k 
        _ -> throwError $ FunctionExpected u

checkEqual :: Expr -> Expr -> Infer ()
checkEqual e1 e2 = do 
    areEq <- equal e1 e2
    unless areEq $ 
        throwError $ TypeMismatch [e1, e2]
    

equal :: Expr -> Expr -> Infer Bool
equal e1 e2 = do 
    n1 <- normalize e1 
    n2 <- normalize e2
    n1 `equal` n2
    where 
        equal e1 e2 = 
            case (e1, e2) of
                (Var v1, Var v2) -> pure $ v1 == v2

                (App e1Fun e1Arg, App e2Fun e2Arg) ->
                    equal e1Fun e2Fun &&^ equal e1Arg e2Arg

                (Universe k1, Universe k2) -> pure $ k1 == k2

                (Pi a1, Pi a2) -> equalAbstraction a1 a2

                (Lam a1, Lam a2) -> equalAbstraction a1 a2 
                
                _ -> pure False

        equalAbstraction (x, t1, e1) (y, t2, e2) = do 
            z <- Var <$> refresh x
            sub1 <- substitute [(x, z)] e1
            sub2 <- substitute [(y, z)] e2
            equal t1 t2 &&^ equal sub1 sub2

normalize :: Expr -> Infer Expr 
normalize expr = case expr of 
    Var v -> do 
        valMay <- lookupValue v 
        case valMay of 
            Nothing -> pure $ Var v 
            Just val -> normalize val 

    App e1 e2 -> do 
        e2 <- normalize e2 
        e1Norm <- normalize e1 
        case e1Norm of 
            Lam (var, _, bodyExpr) -> do 
                sub <- substitute [(var, e2)] bodyExpr
                normalize sub 
            e1 -> pure $ App e1 e2

    Universe k -> pure $ Universe k 

    Pi a -> Pi <$> normalizeAbstraction a

    Lam a -> Lam <$> normalizeAbstraction a
    
normalizeAbstraction :: Abstraction -> Infer Abstraction
normalizeAbstraction (var, t, e) = do 
    t <- normalize t 
    e <- extend var t Nothing $ normalize e
    pure (var, t, e)

