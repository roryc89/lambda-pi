{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Andromeda.Ctx where 

import Andromeda.Expr
import Andromeda.InferType
import Andromeda.Error

import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask, local)
    
-- A context is represented as an associative list which maps a variable `x` to a pair
--    `(t,e)` where `t` is its type and `e` is its value (optional).

-- | The entries in the context are declarations of parameters or definitions.
-- |     A parameter declaration carries its type, while a definition carries the type and
-- |     the defining expression. 
data Declaration 
    = Parameter TypeExpr
    | Definition TypeExpr Expr

data Ctx = Ctx 
    { names :: [Text]
    , decls :: [Declaration]
    }

emptyCtx :: Ctx
emptyCtx = Ctx [] []

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
