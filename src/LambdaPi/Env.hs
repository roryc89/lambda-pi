module LambdaPi.Env (
  Env(..),
  empty,
  lookup,
  remove,
  extend,
  extends,
  merge,
  mergeEnvs,
  singleton,
  keys,
  fromList,
  toList,
) where

import Prelude hiding (lookup)

-- import LambdaPi.Expr
-- import LambdaPi.Type

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Data.Text (Text)
import LambdaPi.Term

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

newtype Env = TypeEnv { types :: Map.Map Name Forall }
  deriving (Eq, Show)

data Forall = Forall [TVar] Term
  deriving (Show, Eq, Ord)
  
empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Name, Forall) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Name, Forall)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Name -> Env -> Maybe Forall
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Name -> Forall -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Forall)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Name, Forall)]
toList (TypeEnv env) = Map.toList env

instance Semigroup Env where
  (<>) = merge

instance Monoid Env where
  mempty = empty

type Name = Text
type TVar = Text