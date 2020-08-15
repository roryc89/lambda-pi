-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Andromeda.Main where 

-- import Control.Monad (when)
-- import Control.Monad.Except
-- import Control.Monad.State
-- import Control.Monad.Reader
-- import Control.Monad.Identity
-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.Text (Text)


-- data TypeError 
--     = TypeMismatch [Expr]
--     | NotAFunction Expr
--     | UnknownVariable Text
--     -- | PatternDoesNotFitType Expr Pattern
--     | Errs TypeError TypeError
--     deriving (Show, Eq)

-- collectErrors :: [TypeError] -> Maybe TypeError
-- collectErrors = foldr go Nothing
--     where
--         go :: TypeError -> Maybe TypeError -> Maybe TypeError
--         go err (Just err2) = Just $ Errs err err2 
--         go err Nothing = Just err

-- -- | Inference state
-- newtype InferState = 
--     InferState 
--         { count :: Int
--         }

-- newInferState :: InferState
-- newInferState = InferState { count = 0 }

-- data Env = Env 
--   { types :: Map.Map Text Expr 
--   , idxedTypes :: Map.Map Int Expr
--   }
--   deriving (Eq, Show)

-- newEnv :: Env 
-- newEnv = Env Map.empty Map.empty 

-- -- | Inference monad
-- type Infer a = 
--   ( ReaderT
--       Env             -- Typing environment
--       ( StateT         -- Inference state
--           InferState
--           (Except TypeError)
--       )
--       a
--   )

-- data Variable 
--     = StringVar Text
--     | GeneratedVar Text Int 
--     | Dummy
--     deriving (Show, Eq)

-- data Expr 
--     = Var Variable
--     | Universe Int
--     | Pi Abstraction
--     | Lam Abstraction
--     | App Expr Expr
--     deriving (Show, Eq)


-- type Abstraction = (Variable, Expr, Expr)

-- refresh :: Variable -> Infer Variable
-- refresh v = do
--     s <- get
--     put s {count = count s + 1}
--     pure $ case v of 
--         StringVar x -> GeneratedVar x $ count s
--         GeneratedVar x _ -> GeneratedVar x $ count s
--         Dummy ->  GeneratedVar "_" $ count s

-- substitute :: [(Variable, Expr)] -> Expr -> Infer Expr
-- substitute s e = case e of 
--     Var v -> pure $ fromMaybe (Var v) $ lookup v s
--     Universe k -> return $ Universe k 
--     Pi a -> Pi <$> substituteAbstraction s a
--     Lam a -> Lam <$> substituteAbstraction s a
--     App e1 e2 -> App <$> substitute s e1 <*> substitute s e2

-- substituteAbstraction :: [(Variable, Expr)] -> Abstraction -> Infer Abstraction
-- substituteAbstraction s (x, t, e) = do 
--     newX <- refresh x
--     x_ <- substitute s t
--     e_ <- substitute ((x, Var newX) : s) e
--     return (newX, x_, e_)
