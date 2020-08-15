{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Andromeda.InferType where 

import Andromeda.Error
import Andromeda.Expr
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)


-- | Inference state
newtype InferState = 
    InferState 
        { count :: Int
        }

newInferState :: InferState
newInferState = InferState { count = 0 }

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
newtype Env = Env 
  { ctx :: Map.Map Variable (TypeExpr, Maybe Expr) 
  }
  deriving (Eq, Show)

newEnv :: Env 
newEnv = Env Map.empty 

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

refresh :: Variable -> Infer Variable
refresh v = do
    s <- get
    put s {count = count s + 1}
    pure $ case v of 
        StringVar x -> GeneratedVar x $ count s
        GeneratedVar x _ -> GeneratedVar x $ count s
        Dummy ->  GeneratedVar "_" $ count s

-- substitute :: [(Variable, Expr)] -> Expr -> Infer Expr
-- substitute s e = case e of 
--     Var v -> pure $ fromMaybe (Var v) $ lookup v s
--     Universe k -> return $ Universe k 
--     Pi a -> Pi <$> substituteAbstraction s a
--     Lam a -> Lam <$> substituteAbstraction s a
--     App e1 e2 -> App <$> substitute s e1 <*> substitute s e2

-- substituteAbstraction :: [(Variable, Expr)] -> Abstraction -> Infer Abstraction
-- substituteAbstraction s (var, t, e) = do 
--     newX <- refresh var
--     var_ <- substitute s t
--     e_ <- substitute ((var, Var newX) : s) e
--     return (newX, var_, e_)
