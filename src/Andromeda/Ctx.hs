{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Andromeda.Ctx where 

import Andromeda.Syntax (Variable, Infer, Expr, Env(..), TypeError(..), TypeExpr)
import qualified Data.Map as Map
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask, local)
    
lookupType :: Variable -> Infer Expr
lookupType v = do 
    Env {ctx} <- ask
    case fst <$> Map.lookup v ctx of 
        Nothing -> throwError $ UnknownVariable v
        Just tipe -> pure tipe

lookupValue :: Variable -> Infer (Maybe Expr)
lookupValue v = do 
    Env {ctx} <- ask
    case snd <$> Map.lookup v ctx of 
        Nothing -> throwError $ UnknownVariable v
        Just val -> pure val 

extend :: Variable -> TypeExpr -> Maybe Expr -> Infer a -> Infer a
extend var tipe value = local 
   (\(Env env) -> Env $ Map.insert var (tipe, value) env)
