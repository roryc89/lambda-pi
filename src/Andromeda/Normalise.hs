module Andromeda.Normalise where

import Andromeda.Ctx 
import Andromeda.Expr 
import Andromeda.Error
import Andromeda.InferType
import Control.Monad.Except

-- | Returns the normal form of an expression
normalForm :: Expr -> Infer Expr
normalForm = normalise False 

-- | Returns the weak head normal form of an expression
whNormalForm :: Expr -> Infer Expr
whNormalForm = normalise True

normalise :: Bool -> Expr -> Infer Expr 
normalise weak e = 
    case e of 
        Var k -> do 
            def <- lookupDefinition k 
            case def of 
                Nothing -> pure e 
                Just e -> normalise weak e

        Universe _ -> pure e
        Pi a -> Pi <$> normaliseAbstraction weak a 
        Lam a -> Lam <$> normaliseAbstraction weak a
        Subst s e -> normalise weak $ subst s e 
        App e1 e2 -> do 
            e1 <- normalise weak e1 
            case e1 of 
                Lam (x, t, e) -> normalise weak $ subst (Dot e2 idSubst) e
                Var _ -> do
                    e2 <- if weak then pure e2 else normalise weak e2
                    pure $ App e1 e2
                App _ _ -> do
                    e2 <- if weak then pure e2 else normalise weak e2
                    pure $ App e1 e2
                _ -> throwError $ FunctionExpected e2


normaliseAbstraction :: Bool -> Abstraction -> Infer Abstraction
normaliseAbstraction weak abs@(x, t, e) = 
    if weak then 
        pure abs 
    else do 
        tNorm <- normalise weak t 
        eNorm <- addParameter x t $ normalise weak t
        pure (x, tNorm, eNorm)
