{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Andromeda.Ctx where 

import Andromeda.Expr
import Andromeda.InferType
import Andromeda.Error

import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask, local)
import Safe (atMay)

lookupType :: Int -> Infer TypeExpr
lookupType k = do 
    Ctx {decls} <- ask
    case atMay decls k of 
        Nothing -> throwError $ UnknownIdx k
        Just (Definition t _) -> pure $ shift (k + 1) t
        Just (Parameter t) -> pure $ shift (k + 1) t


lookupDefinition :: Int -> Infer (Maybe Expr)
lookupDefinition k = do 
    Ctx {decls, names} <- ask
    case atMay decls k of 
        Nothing -> throwError $ UnknownIdx k
        Just (Definition _ e) -> pure $ Just $ shift (k + 1) e
        Just (Parameter _) -> pure Nothing

addParameter :: Text -> TypeExpr -> Infer a -> Infer a
addParameter name tipe = 
    local 
        (\Ctx{..} -> 
            Ctx (name : names) (Parameter tipe : decls)
        )

addDefinition :: Text -> TypeExpr -> Expr -> Infer a -> Infer a
addDefinition name tipe expr = 
    local 
        (\Ctx{..} -> 
            Ctx (name : names) (Definition tipe expr : decls)
        )



